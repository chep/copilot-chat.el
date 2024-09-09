;;; copilot-chat-request.el --- copilot chat request backend -*- lexical-binding:t; indent-tabs-mode: nil -*-

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

;; Request backend for copilot-chat

;;; Code:


(require 'json)
(require 'request)
(require 'copilot-chat-common)

(cl-defun copilot-chat--request-token-cb (&key response
                                          &key data
                                          &allow-other-keys)
  (unless (= (request-response-status-code response) 200)
    (error "http error"))
  (let ((token (alist-get 'access_token data))
        (token-dir (file-name-directory (expand-file-name copilot-chat-github-token-file))))
    (setf (copilot-chat-github-token copilot-chat--instance) token)
    (when (not (file-directory-p token-dir))
      (make-directory token-dir t))
    (with-temp-file copilot-chat-github-token-file
      (insert token))))

(cl-defun copilot-chat--request-code-cb (&key response
                                         &key data
                                         &allow-other-keys)
  "Manage user code reception for github buth."
  (unless (= (request-response-status-code response) 200)
    (error "http error"))
  (let ((device-code (alist-get 'device_code data))
        (user-code (alist-get 'user_code data))
        (verification-uri (alist-get 'verification_uri data)))
    (gui-set-selection 'CLIPBOARD user-code)
    (read-from-minibuffer
     (format "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
             user-code verification-uri))
    (browse-url verification-uri)
    (read-from-minibuffer "Press ENTER after authorizing.")

    (request "https://github.com/login/oauth/access_token"
      :type "POST"
      :headers `(("content-type" . "application/json")
                 ("accept" . "application/json")
                 ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
                 ("editor-version" . "Neovim/0.10.0")
                 ("user-agent" . "CopilotChat.nvim/2.0.0"))
      :data (format "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}" device-code)
      :parser 'json-read
      :sync t
      :complete #'copilot-chat--request-token-cb)))

(defun copilot-chat--request-login()
  "Manage github login."
  (request "https://github.com/login/device/code"
    :type "POST"
    :data "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}"
    :sync t
    :headers `(("content-type" . "application/json")
               ("accept" . "application/json")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0")
               ("editor-version" . "Neovim/0.10.0"))
    :parser 'json-read
    :complete #'copilot-chat--request-code-cb))


(cl-defun copilot-chat--request-renew-token-cb(&key response
                                               &key data
                                               &allow-other-keys)
  (unless (= (request-response-status-code response) 200)
    (error "Authentication error"))
  (setf (copilot-chat-token copilot-chat--instance) data)
  ;; save token in copilot-chat-token-cache file after creating
  ;; folders if needed
  (let ((cache-dir (file-name-directory (expand-file-name copilot-chat-token-cache))))
    (when (not (file-directory-p cache-dir))
      (make-directory  cache-dir t))
    (with-temp-file copilot-chat-token-cache
      (insert (json-encode data)))))

(defun copilot-chat--request-renew-token()
  "Renew session token."
  (request "https://api.github.com/copilot_internal/v2/token"
    :type "GET"
    :headers `(("authorization" . ,(concat "token " (copilot-chat-github-token copilot-chat--instance)))
               ("accept" . "application/json")
               ("editor-version" . "Neovim/0.10.0")
               ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
               ("user-agent" . "CopilotChat.nvim/2.0.0"))
    :parser 'json-read
    :sync t
    :complete #'copilot-chat--request-renew-token-cb))


(defun copilot-chat--request-ask-parser ()
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
    content))

(defun copilot-chat--request-ask (prompt callback)
  (request "https://api.githubcopilot.com/chat/completions"
    :type "POST"
    :headers `(("openai-intent" . "conversation-panel")
               ("content-type" . "application/json")
              ("user-agent" . "CopilotChat.nvim/2.0.0")
              ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
              ("authorization" . ,(concat "Bearer "
                          (alist-get 'token (copilot-chat-token copilot-chat--instance))))
              ("x-request-id" . ,(copilot-chat--uuid))
              ("vscode-sessionid" . ,(copilot-chat-sessionid copilot-chat--instance))
              ("vscode-machineid" . ,(copilot-chat-machineid copilot-chat--instance))
              ("copilot-integration-id" . "vscode-chat")
              ("openai-organization" . "github-copilot")
              ("editor-version" . "Neovim/0.10.0"))
      :data (copilot-chat--create-req  prompt)
      :parser #'copilot-chat--request-ask-parser
      :complete (cl-function
                 (lambda (&key response
                          &key data
                          &allow-other-keys)
                   (unless (= (request-response-status-code response) 200)
                     (error "Authentication error"))
                   (funcall callback data)
                   (setf (copilot-chat-history copilot-chat--instance) (cons (list prompt "assistant") (copilot-chat-history copilot-chat--instance)))
                   (funcall callback copilot-chat--magic)))))

(provide 'copilot-chat-request)
;;; copilot-chat-request.el ends here
