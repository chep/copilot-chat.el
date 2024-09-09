;;; copilot-chat-curl.el --- copilot chat curl ackend -*- lexical-binding: t; indent-tabs-mode: nil -*-

;; Copyright (C) 2024  copilot-chat maintainers

;; The MIT License (MIT)

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;; This is curl backend for copilot-chat code

;;; Code:

(require 'json)
(require 'copilot-chat-common)

(defvar copilot-chat--curl-current-data nil)

;; customs
(defcustom copilot-chat-curl-program "/usr/bin/curl"
  "Curl program to use if `copilot-chat-use-curl' is set."
  :type 'string
  :group 'copilot-chat)


;;variables
(defvar copilot-chat--curl-answer nil)
(defvar copilot-chat--curl-file nil)


;; functions
(defun copilot-chat--curl-parse-github-token()
  "Curl github token request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer
					 :array-type 'list))
		 (token (gethash "access_token" json-data))
		 (token-dir (file-name-directory
					 (expand-file-name copilot-chat-github-token-file))))
	(setf (copilot-chat-github-token copilot-chat--instance) token)
    (when (not (file-directory-p token-dir))
	  (make-directory token-dir t))
    (with-temp-file copilot-chat-github-token-file
	  (insert token))))

(defun copilot-chat--curl-parse-login()
  "Curl login request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer
					 :array-type 'list))
  		 (device-code (gethash "device_code" json-data))
		 (user-code (gethash "user_code" json-data))
		 (verification-uri (gethash "verification_uri" json-data)))
	(gui-set-selection 'CLIPBOARD user-code)
	(read-from-minibuffer
	 (format "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
			 user-code verification-uri))
	(browse-url verification-uri)
	(read-from-minibuffer "Press ENTER after authorizing.")
	(with-temp-buffer
	  (call-process
	   copilot-chat-curl-program
	   nil
	   t
	   nil
	   "https://github.com/login/oauth/access_token"
	   "-s"
  	   "-X" "POST"
	   "-A" "user-agent: CopilotChat.nvim/2.0.0"
  	   "-H" "content-type: application/json"
	   "-H" "accept: application/json"
	   "-H" "editor-plugin-version: CopilotChat.nvim/2.0.0"
	   "-H" "editor-version: Neovim/0.10.0"
	   "-d" (format "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}" device-code))
	  (copilot-chat--curl-parse-github-token))))


(defun copilot-chat--curl-login()
  "Manage github login."
  (with-temp-buffer
	(call-process
	 copilot-chat-curl-program
	 nil
	 t
	 nil
  	 "https://github.com/login/device/code"
	 "-s"
  	 "-X" "POST"
	 "-A" "user-agent: CopilotChat.nvim/2.0.0"
  	 "-H" "content-type: application/json"
	 "-H" "accept: application/json"
	 "-H" "editor-plugin-version: CopilotChat.nvim/2.0.0"
	 "-H" "editor-version: Neovim/0.10.0"
	 "-d" "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}")
	(copilot-chat--curl-parse-login)))


(defun copilot-chat--curl-parse-renew-token()
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let ((json-data (json-parse-buffer
					:object-type 'alist ;need alist to be compatible with
                                        ;copilot-chat-token format
					:array-type 'list))
		(cache-dir (file-name-directory (expand-file-name copilot-chat-token-cache))))
	(setf (copilot-chat-token copilot-chat--instance) json-data)
	;; save token in copilot-chat-token-cache file after creating
	;; folders if needed
    (when (not (file-directory-p cache-dir))
	  (make-directory  cache-dir t))
    (with-temp-file copilot-chat-token-cache
	  (insert (json-encode json-data)))))


(defun copilot-chat--curl-renew-token()
  "Renew session token."
  (with-temp-buffer
	(call-process
	 copilot-chat-curl-program
	 nil
	 t
	 nil
	 "https://api.github.com/copilot_internal/v2/token"
	 "-X" "GET"
	 "-s"
	 "-A" "user-agent: CopilotChat.nvim/2.0.0"
	 "-H" (format "authorization: token %s" (copilot-chat-github-token copilot-chat--instance))
	 "-H" "accept: application/json"
	 "-H" "editor-plugin-version: CopilotChat.nvim/2.0.0"
	 "-H" "editor-version: Neovim/0.10.0")
	(copilot-chat--curl-parse-renew-token)))


(defun copilot-chat--curl-extract-segment (segment)
  "Extract data from an individual line-delimited segment, returning one of:

  - 'empty: if the segment has no data
  - 'partial: if the segment seems to be incomplete, i.e. more data in a future response
  - 'done: if this segment indicates completion (data: [DONE])
  - otherwise, the entire JSON content (data: {...})"
  (cond
   ;; empty
   ((string-empty-p segment) 'empty)
   ;; seems to have a valid prefix
   ((string-prefix-p "data: " segment)
    (let ((data (substring segment 6)))
      (if (string= data "[DONE]")
          ;; the magic done marker
          'done
        ;; not the done marker, so must be "done: {...json...}"
        (condition-case err
            (json-parse-string data :object-type 'alist)
          ;; failure => the segment was probably truncated and we need more data from a future
          ;; response
          (json-parse-error 'partial)
          (json-end-of-file 'partial)))))
   ;; otherwise, the prefix was probably truncated (e.g. "dat", or "data:") => need more data from a
   ;; future response
   (t 'partial)))


(defun copilot-chat--curl-analyze-response (proc string callback)
  ;; The API conceptually sends us big blob of line-deliminated information, e.g.
  ;;
  ;;     data: {"choices":[{...,"delta":{"content":"great"}}],...}
  ;;
  ;;     data: {"choices":[{...,"delta":{"content":"work"}}],...}
  ;;
  ;;     data: [DONE]
  ;;
  ;; We recieve this piecewise, with this function called with `string' as any substring, completely
  ;; ignoring the lines and other rules of the protocol. Thus, this function processes line-by-line
  ;; but needs to be careful to handle partial input any point. We do this by saving a left-over
  ;; line that failed processing to `copilot-chat--curl-current-data' and reading it on the next call.
  ;;
  ;; For instance, this function could be called with three segments like:
  ;;
  ;; 1. "data: {...}\n\ndat" (break in the middle of a "data: " prefix)
  ;; 2. "a: {...}\n\ndata: [D" (break in the middle of some data content)
  ;; 3. "ONE]\n\n"
  ;;
  ;; Those calls will proceed like this:
  ;;
  ;; 1. With segment 1, successfully process the first line (`callback' is called with argument "great"), skip
  ;;    the next empty line, and then fail to process the trailing "dat"; "dat" is saved to
  ;;    `copilot-chat--curl-current-data'.
  ;;
  ;; 2. With segment 2, the value of `copilot-chat--curl-current-data' is first prepended to `string', and
  ;;    processing continues with "data: {...}\n\ndata: [D". Thus, `callback' is called with "work",
  ;;    the next line skipped, and then "data: [D" saved to `copilot-chat--curl-current-data'.
  ;;
  ;; 3. With segment 3, `copilot-chat--curl-current-data' is prepended to `string', resulting in a value of
  ;;    "data: [DONE]\n\n". Thus, `callback' is called with the value of `copilot-chat--magic', and
  ;;    the two trailing empty lines are skipped.
  (when copilot-chat--curl-current-data
    (setq string (concat copilot-chat--curl-current-data string))
    (setq copilot-chat--curl-current-data nil))

  (let ((segments (split-string string "\n")))
    (dolist (segment segments)
      (let ((extracted (copilot-chat--curl-extract-segment segment)))
        (cond
         ;; No data at all, just skip:
         ((eq extracted 'empty)
          nil)
         ;; Data looks truncated, save it for the next segment:
         ((eq extracted 'partial)
          (setq copilot-chat--curl-current-data segment))
         ;; Final segment, all done:
         ((eq extracted 'done)
          (funcall callback copilot-chat--magic)
	  (setf (copilot-chat-history copilot-chat--instance) (cons (list copilot-chat--curl-answer "assistant") (copilot-chat-history copilot-chat--instance)))
	  (setq copilot-chat--curl-answer nil))
         ;; Otherwise, JSON parsed successfully, extract .choices[0].delta.content and pass that along:
         (extracted
          (let* ((choices (alist-get 'choices extracted))
                 (delta (and (> (length choices) 0) (alist-get 'delta (aref choices 0))))
                 (token (and delta (alist-get 'content delta))))
            (when (and token (not (eq token :null)))
              (funcall callback token)
	      (setq copilot-chat--curl-answer (concat copilot-chat--curl-answer token))))))))))


(defun copilot-chat--curl-ask(prompt callback)
  (setq copilot-chat--curl-current-data nil)
  (when copilot-chat--curl-file
	(delete-file copilot-chat--curl-file))
  (setq copilot-chat--curl-file (make-temp-file "copilot-chat"))
  (with-temp-file copilot-chat--curl-file
    (insert (copilot-chat--create-req prompt)))
  (make-process
   :name "copilot-chat-curl"
   :buffer nil
   :filter (lambda (proc string)
             (copilot-chat--curl-analyze-response proc string callback))
   :stderr (get-buffer-create "*copilot-chat-curl-stderr*")
   :command `("curl"
  			  "-X" "POST"
              "https://api.githubcopilot.com/chat/completions"
										;"http://localhost:8080"
  			  "-H" "openai-intent: conversation-panel"
  			  "-H" "content-type: application/json"
  			  "-H" "editor-plugin-version: CopilotChat.nvim/2.0.0"
  			  "-H" ,(concat "authorization: Bearer " (alist-get 'token (copilot-chat-token copilot-chat--instance)))
  			  "-H" ,(concat "x-request-id: " (copilot-chat--uuid))
  			  "-H" ,(concat "vscode-sessionid: " (copilot-chat-sessionid copilot-chat--instance))
  			  "-H" ,(concat "vscode-machineid: " (copilot-chat-machineid copilot-chat--instance))
  			  "-H" "copilot-integration-id: vscode-chat"
  			  "-H" "User-Agent: CopilotChat.nvim/2.0.0"
  			  "-H" "openai-organization: github-copilot"
  			  "-H" "editor-version: Neovim/0.10.0"
  			  "-d" ,(concat "@" copilot-chat--curl-file))))


(provide 'copilot-chat-curl)
;;; copilot-chat-curl.el ends here
