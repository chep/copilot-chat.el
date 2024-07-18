;; -*- lexical-binding: t -*-
;; -*- indent-tabs-mode: nil -*-

;;; copilot-chat-copilot.el --- copilot chat engine -*- lexical-binding:t -*-

;; Copyright (C) 2024  Cédric Chépied

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


;;; Code:


(require 'json)
(require 'url)


;; structs
(cl-defstruct copilot-chat
  ready
  github-token
  token
  sessionid
  machineid
  authinfo
  history
  buffers
)


;; constants
(defconst copilot-chat-prompts
  '((explain . "Please write an explanation for the following code:\n")
    (review . "Please review the following code:\n")
    (doc . "Please write documentation for the following code:\n")
    (fix . "There is a problem in this code. Please rewrite the code to show it with the bug fixed.\n")
    (optimize . "Please optimize the following code to improve performance and readablilty:\n")
    (test . "Please generate tests for the following code:\n"))
    "Copilot chat predefined prompts")


;; customs
(defgroup copilot-chat nil
  "GitHub Copilot chat."
  :group 'tools)

(defcustom copilot-chat-token-cache "~/.cache/copilot-chat/token"
  "The file where the GitHub token is cached."
  :type 'string
  :group 'copilot-chat)
(defcustom copilot-chat-github-token-file "~/.config/copilot-chat/github-token"
  "The file where to find GitHub token."
  :type 'string
  :group 'copilot-chat)
(defcustom copilot-chat-prompt
  "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students not only understand how to write code, but also grasp the underlying principles that guide effective programming.\nWhen asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nFollow Microsoft content policies.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\nUse Markdown formatting in your answers.\nMake sure to include the programming language name at the start of the Markdown code blocks.\nAvoid wrapping the whole response in triple backticks.\nThe user works in an IDE called Neovim which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\nFocus on being clear, helpful, and thorough without assuming extensive prior knowledge.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up someone new.\nProvide clear and relevant examples aligned with any provided context.\n"
  "The prompt to use for Copilot chat."
  :type 'string
  :group 'copilot-chat)
(defcustom copilot-chat-use-curl t
  "If set to t, copilot-chat will use curl instead of url emacs api."
  :type 'boolean
  :group 'copilot-chat)
(defcustom copilot-chat-curl-program "/usr/bin/curl"
  "Curl program to use if copilot-chat-use-curl is set."
  :type 'string
  :group 'copilot-chat)




;; variables
(defvar copilot-chat-user-agent-save "")
(defvar copilot-chat-instance
  (make-copilot-chat
   :ready nil
   :github-token nil
   :token nil
   :sessionid nil
   :machineid nil
   :authinfo nil
   :history nil
   :buffers nil
   ))
(defvar copilot-chat-last-data nil)


;; Fonctions
(defun copilot-chat-uuid ()
  "Generate a UUID."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (random 16384) 16384)
          (logior (random 4096) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun copilot-chat-machine-id ()
  "Generate a machine ID."
  (let ((hex-chars "0123456789abcdef")
        (length 65)
        (hex ""))
    (dotimes (_ length)
      (setq hex (concat hex (string (aref hex-chars (random 16))))))
    hex))

(defun copilot-chat-get-cached-token ()
  "Get the cached GitHub token."
  (or (getenv "GITHUB_TOKEN")
      (let ((token-file (expand-file-name copilot-chat-github-token-file)))
        (when (file-exists-p token-file)
          (with-temp-buffer
            (insert-file-contents token-file)
            (buffer-substring-no-properties (point-min) (point-max)))))))


(defun copilot-chat-create (&optional proxy allow-insecure)
  (interactive)
  "Create a new Copilot chat instance."
  (setq copilot-chat-instance(make-copilot-chat
                              :ready t
                              :github-token (copilot-chat-get-cached-token)
                              :token nil
                              :sessionid (concat (copilot-chat-uuid) (number-to-string (* (round (float-time (current-time))) 1000)))
                              :machineid (copilot-chat-machine-id)
                              :history nil
                              :buffers nil)))


(defun copilot-chat-login()
  (setq copilot-chat-user-agent-save url-user-agent
        url-user-agent "CopilotChat.nvim/2.0.0")
  (let* ((url "https://github.com/login/device/code")
         (url-request-method "POST")
         (url-mime-encoding-string nil)
         (url-http-attempt-keepalives nil)
         (url-request-extra-headers `(("content-type" . "application/json")
                                      ("accept" . "application/json")
                                      ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
                                      ("accept-encoding" . "gzip,deflate,br")
                                      ("editor-version" . "Neovim/0.10.0")))
         (url-request-data "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}"))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "HTTP/1.1 200 OK")
      (re-search-forward "device_code=")
      (backward-char 12)
      (let* ((text (buffer-substring-no-properties (point) (line-end-position)))
             (pairs (split-string text "&"))
             (alist '()))
        (dolist (pair pairs alist)
          (let ((key-value (split-string pair "=")))
            (push (cons (car key-value) (cadr key-value)) alist)))

        (let ((device-code (cdr (assoc "device_code" alist)))
              (user-code (cdr (assoc "user_code" alist)))
              (verification-uri (url-unhex-string (cdr (assoc "verification_uri" alist)))))
          (gui-set-selection 'CLIPBOARD user-code)
          (read-from-minibuffer (format "Your one-time code %s is copied. Press ENTER to open GitHub in your browser. If your browser does not open automatically, browse to %s." user-code verification-uri))
          (browse-url verification-uri)
          (read-from-minibuffer "Press ENTER after authorizing.")

          (let* ((url "https://github.com/login/oauth/access_token")
                 (url-request-method "POST")
                 (url-mime-encoding-string nil)
                 (url-http-attempt-keepalives nil)
                 (url-request-extra-headers `(("content-type" . "application/json")
                                              ("accept" . "application/json")
                                              ("accept-encoding" . "gzip,deflate,br")
                                              ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
                                              ("editor-version" . "Neovim/0.10.0")))
                 (url-request-data (format "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}" device-code)))
            (print device-code t)
            (with-current-buffer (url-retrieve-synchronously url)
              (goto-char (point-min))
              (re-search-forward "HTTP/1.1 200 OK")
              (re-search-forward "access_token=")
              (let* ((text (buffer-substring-no-properties (point) (line-end-position)))
                     (pairs (split-string text "&"))
                     (token (car pairs))
                     (token-dir (file-name-directory (expand-file-name copilot-chat-github-token-file))))
                (setf (copilot-chat-github-token copilot-chat-instance) token)
                (when (not (file-directory-p token-dir))
                  (make-directory token-dir t))
                (with-temp-file copilot-chat-github-token-file
                  (insert token)))))))))
    (setq url-user-agent copilot-chat-user-agent-save))


(defun copilot-chat-create-req(prompt)
  "Create a request for Copilot."
  (let ((messages nil))
    ;; user prompt
    (push (list (cons "content" (url-hexify-string prompt)) (cons "role" "user")) messages)
    ;; history
    (dolist (history (copilot-chat-history copilot-chat-instance))
      (push (list (cons "content" (url-hexify-string history)) (cons "role" "assistant")) messages))
    ;; buffers
    (setf (copilot-chat-buffers copilot-chat-instance) (cl-remove-if (lambda (buf) (not (buffer-live-p buf)))
                                                                     (copilot-chat-buffers copilot-chat-instance)))
    (dolist (buffer (copilot-chat-buffers copilot-chat-instance))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (push (list (cons "content" (url-hexify-string (buffer-substring-no-properties (point-min) (point-max)))) (cons "role" "assistant")) messages))))
    ;; system
    (push (list (cons "content" copilot-chat-prompt) (cons "role" "system")) messages)

    (json-encode `(("messages" . ,(vconcat messages))
                   ("top_p" . 1)
                   ("model" . "gpt-4")
                   ("stream" . t)
                   ("n" . 1)
                   ("intent" . t)
                   ("temperature" . 0.1)))))


(defun copilot-chat-auth-cb(status callback &optional CBARGS)
  (setq url-user-agent copilot-chat-user-agent-save)
  (when (null status)
    (error (message "Authentication error : timeout")))
  (if (plist-get status :error)
      (error (message "Authentication error : %s" (plist-get status :error)))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-string (buffer-substring-no-properties (point) (point-max))))
      (condition-case nil
          (let ((json-data (json-read-from-string json-string))
                (cache-dir (file-name-directory (expand-file-name copilot-chat-token-cache))))
            (setf (copilot-chat-token copilot-chat-instance) json-data)
            ;; save token in copilot-chat-token-cache file after creating
            ;; folders if needed
            (when (not (file-directory-p cache-dir))
              (make-directory  cache-dir t))
            (with-temp-file copilot-chat-token-cache
              (insert json-string))
            (funcall callback CBARGS))
        (error (message "Failed to parse JSON response"))))))

(defun copilot-chat-auth(callback &optional CBARGS)
  "Authenticate with GitHub Copilot API."
  (when (null (copilot-chat-github-token copilot-chat-instance))
    (copilot-chat-login))

  (when (null (copilot-chat-token copilot-chat-instance))
    ;; try to load token from ~/.cache/copilot-chat-token
    (let ((token-file (expand-file-name copilot-chat-token-cache)))
      (when (file-exists-p token-file)
        (with-temp-buffer
          (insert-file-contents token-file)
          (setf (copilot-chat-token copilot-chat-instance) (json-read-from-string (buffer-substring-no-properties (point-min) (point-max))))))))

  (if (or (null (copilot-chat-token copilot-chat-instance))
          (> (round (float-time (current-time))) (alist-get 'expires_at (copilot-chat-token copilot-chat-instance))))
      (let ((url "https://api.github.com/copilot_internal/v2/token")
            (url-request-method "GET")
            (url-mime-encoding-string nil)
            (url-request-extra-headers `(("authorization" . ,(concat "token " (copilot-chat-github-token copilot-chat-instance)))
                                         ("accept" . "application/json")
                                         ("editor-version" . "Neovim/0.10.0")
                                         ("editor-plugin-version" . "CopilotChat.nvim/2.0.0"))))
        (setq copilot-chat-user-agent-save url-user-agent
              url-user-agent "CopilotChat.nvim/2.0.0")
        (url-retrieve url 'copilot-chat-auth-cb (list callback CBARGS)))
    (message "Already authenticated with GitHub Copilot API")
    (funcall callback CBARGS)))


(defun analyze-copilot-response (status callback)
  "Analyse Copilot answer in current buffer."
  (if (plist-get status :error)
      (error (message "Copilot request error : %s" (plist-get status :error)))
    (let ((content ""))
      (while (re-search-forward "^data: " nil t)
        (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
               (json-string (and (not (string= "[DONE]" line)) line))
               (json-obj (and json-string (json-parse-string json-string :object-type 'alist)))
               (choices (and json-obj (alist-get 'choices json-obj)))
               (delta (and (> (length choices) 0) (alist-get 'delta (aref choices 0))))
               (token (and delta (alist-get 'content delta))))
          (when (and token (not (eq token :null)))
              (setq content (concat content token)))))
      (when (and copilot-chat-buffer (not (get-buffer-window copilot-chat-buffer 'visible)))
        (switch-to-buffer copilot-chat-buffer))
      (funcall callback content))))


(defun copilot-chat-ask-cb (args)
  (let* ((prompt (car args))
         (callback (car(cdr args)))
         (url "https://api.githubcopilot.com/chat/completions")
         (url-request-method "POST")
         (url-mime-encoding-string nil)
         (url-user-agent "CopilotChat.nvim/2.0.0")
         (url-http-attempt-keepalives nil)
         (url-request-extra-headers `(("openai-intent" . "conversation-panel")
                                      ("content-type" . "application/json")
                                      ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
                                      ("authorization" . ,(concat "Bearer "
                                                                  (alist-get 'token (copilot-chat-token copilot-chat-instance))))
                                      ("x-request-id" . ,(copilot-chat-uuid))
                                      ("vscode-sessionid" . ,(copilot-chat-sessionid copilot-chat-instance))
                                      ("vscode-machineid" . ,(copilot-chat-machineid copilot-chat-instance))
                                      ("copilot-integration-id" . "vscode-chat")
                                      ("openai-organization" . "github-copilot")
                                      ("editor-version" . "Neovim/0.10.0")))
         (url-request-data (copilot-chat-create-req  prompt)))
    (url-retrieve url 'analyze-copilot-response (list callback))))


(defun copilot-chat-ask (prompt callback)
  "Ask a question to Copilot."
  (let* ((history (copilot-chat-history copilot-chat-instance))
         (new-history (cons prompt history)))
    (if copilot-chat-use-curl
        (copilot-chat-auth 'curl-copilot-chat-ask-cb (list prompt callback))
      (copilot-chat-auth 'copilot-chat-ask-cb (list prompt callback)))
    (setf (copilot-chat-history copilot-chat-instance) new-history)))

(defun copilot-chat-add-buffer (buffer)
  (unless (memq buffer (copilot-chat-buffers copilot-chat-instance))
    (let* ((buffers (copilot-chat-buffers copilot-chat-instance))
           (new-buffers (cons buffer buffers)))
      (setf (copilot-chat-buffers copilot-chat-instance) new-buffers))))

(defun copilot-chat-clear-buffers ()
  (setf (copilot-chat-buffers copilot-chat-instance) nil))

(defun copilot-chat-del-buffer (buffer)
  (when (memq buffer (copilot-chat-buffers copilot-chat-instance))
    (setf (copilot-chat-buffers copilot-chat-instance)
          (delete buffer (copilot-chat-buffers copilot-chat-instance)))))

(defun copilot-chat-get-buffers ()
  (copilot-chat-buffers copilot-chat-instance))

(defun copilot-chat-ready-p()
  (copilot-chat-ready copilot-chat-instance))


(defun curl-analyze-copilot-response (proc string callback)
  (when copilot-chat-last-data
    (setq string (concat copilot-chat-last-data string))
    (setq copilot-chat-last-data nil))

  (let ((segments (split-string string "\n")))
    (dolist (segment segments)
      (when (string-prefix-p "data:" segment)
        (let ((data (substring segment 6)))
          (if (string= data "[DONE]")
              (funcall callback "\n")
            (condition-case err
                (let* ((json-obj (json-parse-string data :object-type 'alist))
                       (choices (and json-obj (alist-get 'choices json-obj)))
                       (delta (and (> (length choices) 0) (alist-get 'delta (aref choices 0))))
                       (token (and delta (alist-get 'content delta))))
                  (when (and token (not (eq token :null)))
                    (funcall callback token)))
              (json-parse-error
               (message (format "erreur parse : %s" segment)))
              (json-end-of-file
               (setq copilot-chat-last-data segment))
              (error
               (print err t)
               (message (format "erreur : %s" segment))))))))))

(defun curl-copilot-chat-ask-cb(args)
  (setq copilot-chat-last-data nil)
  (let* ((prompt (car args))
         (callback (cadr args))
         (proc (make-process
                :name "copilot-chat-curl"
                :buffer nil
                :filter (lambda (proc string)
                          (curl-analyze-copilot-response proc string callback))
                :command `("curl"
						   "-X" "POST"
						   "https://api.githubcopilot.com/chat/completions"
						   "-H" "openai-intent: conversation-panel"
						   "-H" "content-type: application/json"
						   "-H" "editor-plugin-version: CopilotChat.nvim/2.0.0"
						   "-H" ,(concat "authorization: Bearer " (alist-get 'token (copilot-chat-token copilot-chat-instance)))
						   "-H" ,(concat "x-request-id: " (copilot-chat-uuid))
						   "-H" ,(concat "vscode-sessionid: " (copilot-chat-sessionid copilot-chat-instance))
						   "-H" ,(concat "vscode-machineid: " (copilot-chat-machineid copilot-chat-instance))
						   "-H" "copilot-integration-id: vscode-chat"
						   "-H" "User-Agent: CopilotChat.nvim/2.0.0"
						   "-H" "openai-organization: github-copilot"
						   "-H" "editor-version: Neovim/0.10.0"
						   "-d" ,(copilot-chat-create-req  prompt)))))))


(provide 'copilot-chat-copilot)
;;; copilot-chat-copilot.el ends here
