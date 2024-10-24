;;; copilot-chat --- copilot-chat-curl.el --- copilot chat curl backend -*- indent-tabs-mode: nil; lexical-binding: t -*-

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
(defcustom copilot-chat-curl-program "curl"
  "Curl program to use if `copilot-chat-use-curl' is set."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-curl-proxy nil
  "Curl will use this proxy if defined.
The proxy string can be specified with a protocol:// prefix.  No protocol
specified or http:// it is treated as an HTTP proxy.  Use socks4://,
socks4a://, socks5:// or socks5h:// to request a specific SOCKS version
to be used.

Unix domain sockets are supported for socks proxy.  Set localhost for the
host part.  e.g. socks5h://localhost/path/to/socket.sock

HTTPS proxy support works set with the https:// protocol prefix for
OpenSSL and GnuTLS.  It also works for BearSSL, mbedTLS, rustls,
Schannel, Secure Transport and wolfSSL (added in 7.87.0).

Unrecognized and unsupported proxy protocols cause an error.  Ancient
curl versions ignored unknown schemes and used http:// instead.

If the port number is not specified in the proxy string, it is assumed
to be 1080.

This option overrides existing environment variables that set the proxy
to use.  If there is an environment variable setting a proxy, you can set
proxy to \"\" to override it.

User and password that might be provided in the proxy string are URL
decoded by curl. This allows you to pass in special characters such as @
by using %40 or pass in a colon with %3a."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-curl-proxy-insecure nil
  "Insecure flag for copilot-chat proxy with curl backend.
Every secure connection curl makes is verified to be secure before the
transfer takes place.  This option makes curl skip the verification step
with a proxy and proceed without checking."
  :type 'boolean
  :group 'copilot-chat)

(defcustom copilot-chat-curl-proxy-user-pass nil
  "User password for copilot-chat proxy with curl backend.
Specify the username and password <user:password> to use for proxy
authentication."
  :type 'boolean
  :group 'copilot-chat)


;;variables
(defvar copilot-chat--curl-answer nil)
(defvar copilot-chat--curl-file nil)


;; functions
(defun copilot-chat--curl-call-process(address method data &rest args)
  "Call curl synchronously.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Arguments ARGS are additional arguments to pass to curl."
  (let ((curl-args (append
                    (list address
                          "-s"
                          "-X" (if (eq method 'post) "POST" "GET")
                          "-A" "user-agent: CopilotChat.nvim/2.0.0"
  	                      "-H" "content-type: application/json"
	                      "-H" "accept: application/json"
	                      "-H" "editor-plugin-version: CopilotChat.nvim/2.0.0"
	                      "-H" "editor-version: Neovim/0.10.0")
                    (when data (list "-d" data))
                    (when copilot-chat-curl-proxy (list "-x" copilot-chat-curl-proxy))
                    (when copilot-chat-curl-proxy-insecure (list "--proxy-insecure"))
                    (when copilot-chat-curl-proxy-user-pass
                      (list
                       "-U"
                       copilot-chat-curl-proxy-user-pass))
                    args)))
    (apply #'call-process
           copilot-chat-curl-program
           nil
           t
	       nil
           curl-args)))

(defun copilot-chat--curl-make-process(address method data filter &rest args)
  "Call curl asynchronously.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Argument FILTER is the function called to parse data.
Optional argument ARGS are additional arguments to pass to curl."
  (let ((command (append
                    (list copilot-chat-curl-program
                          address
                          "-s"
                          "-X" (if (eq method 'post) "POST" "GET")
                          "-A" "user-agent: CopilotChat.nvim/2.0.0"
  	                      "-H" "content-type: application/json"
	                      "-H" "accept: application/json"
	                      "-H" "editor-plugin-version: CopilotChat.nvim/2.0.0"
	                      "-H" "editor-version: Neovim/0.10.0")
                    (when data (list "-d" data))
                    (when copilot-chat-curl-proxy (list "-x" copilot-chat-curl-proxy))
                    (when copilot-chat-curl-proxy-insecure (list "--proxy-insecure"))
                    (when copilot-chat-curl-proxy-user-pass
                      (list
                       "-U"
                       copilot-chat-curl-proxy-user-pass))
                    args)))
    (make-process
     :name "copilot-chat-curl"
     :buffer nil
     :filter filter
     :stderr (get-buffer-create "*copilot-chat-curl-stderr*")
     :command command)))

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
      (copilot-chat--curl-call-process
	   "https://github.com/login/oauth/access_token"
       'post
       (format "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}" device-code))
	  (copilot-chat--curl-parse-github-token))))


(defun copilot-chat--curl-login()
  "Manage github login."
  (with-temp-buffer
    (copilot-chat--curl-call-process
     "https://github.com/login/device/code"
     'post
     "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}")
	(copilot-chat--curl-parse-login)))


(defun copilot-chat--curl-parse-renew-token()
  "Curl renew token request parsing."
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
    (copilot-chat--curl-call-process
	 "https://api.github.com/copilot_internal/v2/token"
     'get
     nil
	 "-H" (format "authorization: token %s" (copilot-chat-github-token copilot-chat--instance)))
	(copilot-chat--curl-parse-renew-token)))


(defun copilot-chat--curl-extract-segment (segment)
  "Extract data from an individual line-delimited SEGMENT, returning one of:
- `empty` if the segment has no data
- `partial`: if the segment seems to be incomplete, i.e. more data in a
  future response
- `done`: if this segment indicates completion (data: [DONE])
- otherwise, the entire JSON content (data: {...})
Argument SEGMENT is data segment to parse."
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
        (condition-case _err
            (json-parse-string data :object-type 'alist)
          ;; failure => the segment was probably truncated and we need more data from a future
          ;; response
          (json-parse-error 'partial)
          (json-end-of-file 'partial)))))
   ;; otherwise, try parsing the segment as a non-prefixed json (such as in
   ;; error responses) When even this fails, then we have a partial response
   ;; that was probably truncated (e.g. "dat", or "data:") => need more data
   ;; from a future response
   (t
    (condition-case _err
        (json-parse-string segment :object-type 'alist)
      (error 'partial)))))


(defun copilot-chat--curl-analyze-response (_proc string callback no-history)
  "Analyse curl resonse.
Argument PROC is curl process.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data."
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
          (unless no-history
            (setf (copilot-chat-history copilot-chat--instance) (cons (list copilot-chat--curl-answer "assistant") (copilot-chat-history copilot-chat--instance))))
          (setq copilot-chat--curl-answer nil))

         ;; Otherwise, JSON parsed successfully
         (extracted
          (cond
           ;; extract .choices[0].delta.content and pass that along:
           ((alist-get 'choices extracted)
            (let* ((choices (alist-get 'choices extracted))
                   (delta (and (> (length choices) 0) (alist-get 'delta (aref choices 0))))
                   (token (and delta (alist-get 'content delta))))
              (when (and token (not (eq token :null)))
                (funcall callback token)
                (setq copilot-chat--curl-answer (concat copilot-chat--curl-answer token)))))

           ;; display .error.message in the chat.
           ((alist-get 'error extracted)
            (let* ((err-response (alist-get 'error extracted))
                   (err-message (alist-get 'message err-response))
                   (answer (format "Error: %s" err-message)))
              (message answer)
              (funcall callback answer)
              (funcall callback copilot-chat--magic)
              ;; Add an empty response to the chat history to avoid confusing the assistant with its own error messages...
              (setf (copilot-chat-history copilot-chat--instance) (cons (list "" "assistant") (copilot-chat-history copilot-chat--instance)))
              (setq copilot-chat--curl-answer nil)))
           ;; Fallback -- nag developers about possibly unhandled payloads
           (t
            (warn "Unhandled message from copilot: %S" extracted)))))))))


(defun copilot-chat--curl-ask(prompt callback out-of-context)
  "Ask a question to Copilot using curl backend.
Argument PROMPT is the prompt to send to copilot.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT is a boolean to indicate if the prompt is out of context."
  (setq copilot-chat--curl-current-data nil)
  (when copilot-chat--curl-file
	(delete-file copilot-chat--curl-file))
  (setq copilot-chat--curl-file (make-temp-file "copilot-chat"))
  (with-temp-file copilot-chat--curl-file
    (insert (copilot-chat--create-req prompt out-of-context)))
  (copilot-chat--curl-make-process
   "https://api.githubcopilot.com/chat/completions"
    'post
    (concat "@" copilot-chat--curl-file)
    (lambda (proc string)
      (copilot-chat--curl-analyze-response proc string callback out-of-context))
    "-H" "openai-intent: conversation-panel"
  	"-H" (concat "authorization: Bearer " (alist-get 'token (copilot-chat-token copilot-chat--instance)))
  	"-H" (concat "x-request-id: " (copilot-chat--uuid))
  	"-H" (concat "vscode-sessionid: " (copilot-chat-sessionid copilot-chat--instance))
  	"-H" (concat "vscode-machineid: " (copilot-chat-machineid copilot-chat--instance))
  	"-H" "copilot-integration-id: vscode-chat"))


(provide 'copilot-chat-curl)
;;; copilot-chat-curl.el ends here
