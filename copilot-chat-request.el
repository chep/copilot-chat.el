;;; copilot-chat --- copilot-chat-request.el --- copilot chat request backend -*- lexical-binding: t; -*-

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

(require 'request)

(require 'copilot-chat-body)
(require 'copilot-chat-common)
(require 'copilot-chat-connection)
(require 'copilot-chat-backend)

(cl-defun
 copilot-chat--request-token-cb (&key response &key data &allow-other-keys)
 "Manage token reception for github auth.
Argument DATA is whatever PARSER function returns, or nil.
Argument RESPONSE is request-response object."
 (unless (= (request-response-status-code response) 200)
   (error "Http error"))
 (let ((token (alist-get 'access_token data))
       (token-dir
        (file-name-directory
         (expand-file-name copilot-chat-github-token-file))))
   (setf (copilot-chat-connection-github-token copilot-chat--connection) token)
   (when (not (file-directory-p token-dir))
     (make-directory token-dir t))
   (with-temp-file copilot-chat-github-token-file
     (insert token))))

(cl-defun
 copilot-chat--request-code-cb (&key response &key data &allow-other-keys)
 "Manage user code reception for github buth.
Argument RESPONSE is request-response object.
Argument DATA is whatever PARSER function returns, or nil."
 (unless (= (request-response-status-code response) 200)
   (error "Http error"))
 (let ((device-code (alist-get 'device_code data))
       (user-code (alist-get 'user_code data))
       (verification-uri (alist-get 'verification_uri data)))
   (gui-set-selection 'CLIPBOARD user-code)
   (read-from-minibuffer
    (format
     "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
     user-code verification-uri))
   (browse-url verification-uri)
   (read-from-minibuffer "Press ENTER after authorizing.")

   (request
    "https://github.com/login/oauth/access_token"
    :type "POST"
    :headers
    `(("content-type" . "application/json")
      ("accept" . "application/json")
      ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
      ("editor-version" . "Neovim/0.10.0")
      ("user-agent" . "CopilotChat.nvim/2.0.0"))
    :data
    (format
     "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}"
     device-code)
    :parser
    (apply-partially 'json-parse-buffer
                     :object-type 'alist
                     :false-object
                     :json-false)
    :sync t
    :complete #'copilot-chat--request-token-cb)))

(defun copilot-chat--request-login ()
  "Manage github login."
  (request
   "https://github.com/login/device/code"
   :type "POST"
   :data "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}"
   :sync t
   :headers
   `(("content-type" . "application/json")
     ("accept" . "application/json")
     ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
     ("user-agent" . "CopilotChat.nvim/2.0.0")
     ("editor-version" . "Neovim/0.10.0"))
   :parser
   (apply-partially 'json-parse-buffer
                    :object-type 'alist
                    :false-object
                    :json-false)
   :complete #'copilot-chat--request-code-cb))


(cl-defun
 copilot-chat--request-renew-token-cb
 (&key response &key data &allow-other-keys)
 "Renew token callback.
Argument RESPONSE is request-response object.
Argument DATA is whatever PARSER function returns, or nil."
 (unless (= (request-response-status-code response) 200)
   (error "Authentication error"))
 (setf (copilot-chat-connection-token copilot-chat--connection) data)
 ;; save token in copilot-chat-token-cache file after creating
 ;; folders if needed
 (let ((cache-dir
        (file-name-directory (expand-file-name copilot-chat-token-cache))))
   (when (not (file-directory-p cache-dir))
     (make-directory cache-dir t))
   (with-temp-file copilot-chat-token-cache
     (insert (json-serialize data :false-object :json-false)))))

(defun copilot-chat--request-renew-token ()
  "Renew session token."
  (request
   "https://api.github.com/copilot_internal/v2/token"
   :type "GET"
   :headers
   `(("authorization" .
      ,(concat
        "token "
        (copilot-chat-connection-github-token copilot-chat--connection)))
     ("accept" . "application/json")
     ("editor-version" . "Neovim/0.10.0")
     ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
     ("user-agent" . "CopilotChat.nvim/2.0.0"))
   :parser
   (apply-partially 'json-parse-buffer
                    :object-type 'alist
                    :false-object
                    :json-false)
   :sync t
   :complete #'copilot-chat--request-renew-token-cb))


(defun copilot-chat--request-ask-parser ()
  "Parser for copilot chat answer."
  (let ((content ""))
    (while (re-search-forward "^data: " nil t)
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
             (json-string (and (not (string= "[DONE]" line)) line))
             (json-obj
              (and json-string
                   (json-parse-string json-string
                                      :object-type 'alist
                                      :false-object
                                      :json-false)))
             (choices (and json-obj (alist-get 'choices json-obj)))
             (delta
              (and (> (length choices) 0) (alist-get 'delta (aref choices 0))))
             (token (and delta (alist-get 'content delta))))
        (when (and token (not (eq token :null)))
          (setq content (concat content token)))))
    content))

(defun copilot-chat--request-ask-non-stream-parser ()
  "Parser for copilot chat answer.
Non-streaming version."
  (let ((content ""))
    (let* ((json-string
            (buffer-substring-no-properties (point-min) (point-max)))
           (json-obj
            (and json-string
                 (json-parse-string json-string
                                    :object-type 'alist
                                    :false-object
                                    :json-false)))
           (choices (and json-obj (alist-get 'choices json-obj)))
           (message
            (and (> (length choices) 0) (alist-get 'message (aref choices 0))))
           (token (and message (alist-get 'content message))))
      (when (and token (not (eq token :null)))
        (setq content (concat content token))))
    content))

(defun copilot-chat--request-ask (instance prompt callback out-of-context)
  "Ask a question to Copilot using request backend.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the prompt to send to copilot.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT is a boolean to indicate
if the prompt is out of context."
  ;; Start spinner if available (only for instances with chat buffers)
  (when (and (fboundp 'copilot-chat--spinner-start)
             (buffer-live-p (copilot-chat-chat-buffer instance)))
    (copilot-chat--spinner-start instance))

  ;; Initialize answer accumulator
  (let ((full-response ""))
    (setf (copilot-chat--backend instance)
          (request
           "https://api.githubcopilot.com/chat/completions"
           :type "POST"
           :headers
           `(("openai-intent" . "conversation-panel")
             ("content-type" . "application/json")
             ("user-agent" . "CopilotChat.nvim/2.0.0")
             ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
             ("Copilot-Vision-Request" .
              ,(if (copilot-chat-uses-vision instance)
                   "true"
                 "false"))
             ("authorization" .
              ,(concat
                "Bearer "
                (alist-get
                 'token
                 (copilot-chat-connection-token copilot-chat--connection))))
             ("x-request-id" . ,(copilot-chat--uuid))
             ("vscode-sessionid"
              .
              ,(copilot-chat-connection-sessionid copilot-chat--connection))
             ("vscode-machineid"
              .
              ,(copilot-chat-connection-machineid copilot-chat--connection))
             ("copilot-integration-id" . "vscode-chat")
             ("openai-organization" . "github-copilot")
             ("editor-version" . "Neovim/0.10.0"))
           :data (copilot-chat--create-req instance prompt out-of-context)
           :parser
           (if (copilot-chat--instance-support-streaming instance)
               #'copilot-chat--request-ask-parser
             #'copilot-chat--request-ask-non-stream-parser)
           :complete
           (cl-function
            (lambda (&key response &key data &allow-other-keys)
              ;; Stop spinner when complete
              (when (fboundp 'copilot-chat--spinner-stop)
                (copilot-chat--spinner-stop instance))

              (setf (copilot-chat--backend instance) nil)
              (unless (= (request-response-status-code response) 200)
                (let ((error-msg
                       (format "Process interrupted: %s"
                               (request-response-status-code response))))
                  (funcall callback instance error-msg)
                  (funcall callback instance copilot-chat--magic)
                  (error error-msg)))

              ;; Update full response and call callback with final magic token
              (setq full-response (concat full-response data))
              (funcall callback instance data)
              (funcall callback instance copilot-chat--magic)
              (unless out-of-context
                (setf (copilot-chat-history instance)
                      (cons
                       (list prompt "assistant")
                       (copilot-chat-history instance))))))
           :status-code
           '((400 .
                  (lambda (&rest _)
                    (when (fboundp 'copilot-chat--spinner-stop)
                      (copilot-chat--spinner-stop instance))
                    (let ((error-msg "Bad request. Please check your input."))
                      (funcall callback instance error-msg)
                      (funcall callback instance copilot-chat--magic))))
             (401 .
                  (lambda (&rest _)
                    (when (fboundp 'copilot-chat--spinner-stop)
                      (copilot-chat--spinner-stop instance))
                    (let ((error-msg
                           "Authentication error. Please re-authenticate."))
                      (funcall callback instance error-msg)
                      (funcall callback instance copilot-chat--magic))))
             (429 .
                  (lambda (&rest _)
                    (when (fboundp 'copilot-chat--spinner-stop)
                      (copilot-chat--spinner-stop instance))
                    (let ((error-msg
                           "Rate limit exceeded. Please try again later."))
                      (funcall callback instance error-msg)
                      (funcall callback instance copilot-chat--magic))))
             (500 .
                  (lambda (&rest _)
                    (when (fboundp 'copilot-chat--spinner-stop)
                      (copilot-chat--spinner-stop instance))
                    (let ((error-msg "Server error. Please try again later."))
                      (funcall callback instance error-msg)
                      (funcall callback instance copilot-chat--magic)))))
           :error
           (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (when (fboundp 'copilot-chat--spinner-stop)
                (copilot-chat--spinner-stop instance))
              (let ((error-msg (format "Request error: %S" error-thrown)))
                (funcall callback instance error-msg)
                (funcall callback instance copilot-chat--magic))))))))

(defun copilot-chat--get-headers ()
  "Get headers for Copilot API requests."
  `(("openai-intent" . "conversation-panel")
    ("content-type" . "application/json")
    ("accept" . "application/json")
    ("user-agent" . "CopilotChat.nvim/2.0.0")
    ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
    ("authorization" .
     ,(concat
       "Bearer "
       (alist-get
        'token (copilot-chat-connection-token copilot-chat--connection))))
    ("x-request-id" . ,(copilot-chat--uuid))
    ("vscode-sessionid"
     .
     ,(copilot-chat-connection-sessionid copilot-chat--connection))
    ("vscode-machineid"
     .
     ,(copilot-chat-connection-machineid copilot-chat--connection))
    ("copilot-integration-id" . "vscode-chat")
    ("openai-organization" . "github-copilot")
    ("editor-version" . "Neovim/0.10.0")))

(cl-defun
 copilot-chat--request-models-cb (&key response &key data &allow-other-keys)
 "Handle models response from Copilot API.
Argument DATA is the parsed JSON response.
Argument RESPONSE is request-response object."
 (unless (= (request-response-status-code response) 200)
   (error "Failed to fetch models: %s" (request-response-status-code response)))

 (let* ((models-vector (alist-get 'data data))
        (models (append models-vector nil)) ; Convert vector to list
        (chat-models nil))
   ;; Filter for chat models and extract capabilities
   (dolist (model models)
     (when (and (alist-get 'capabilities model)
                (equal
                 (alist-get 'type (alist-get 'capabilities model)) "chat"))
       (push model chat-models)))

   (when copilot-chat-debug
     (message "Fetched %d models" (length chat-models))
     (message "Models: %s" chat-models))

   ;; Store models in instance and return them
   (let ((sorted-models (nreverse chat-models)))
     (setf (copilot-chat-connection-models copilot-chat--connection)
           sorted-models)

     ;; Cache models to disk
     (copilot-chat--save-models-to-cache sorted-models)

     ;; Enable policies for models if needed
     (dolist (model sorted-models)
       (when (and (alist-get 'policy model)
                  (equal
                   (alist-get 'state (alist-get 'policy model)) "unconfigured"))
         (copilot-chat--request-enable-model-policy (alist-get 'id model))))

     ;; Return the models list for immediate use
     sorted-models)))

(defun copilot-chat--request-enable-model-policy (model-id)
  "Enable policy for MODEL-ID."
  (let ((url (format "https://api.githubcopilot.com/models/%s/policy" model-id))
        (headers (copilot-chat--get-headers))
        (data
         (json-serialize '((state . "enabled")) :false-object :json-false)))
    (when copilot-chat-debug
      (message "Enabling policy for model %s" model-id))
    (request
     url
     :type "POST"
     :headers headers
     :data data
     :parser
     (apply-partially 'json-parse-buffer
                      :object-type 'alist
                      :false-object
                      :json-false))))

(defun copilot-chat--request-models (&optional quiet)
  "Fetch available models from Copilot API.
Optional argument QUIET suppresses user messages when non-nil."
  (let ((url "https://api.githubcopilot.com/models")
        (headers (copilot-chat--get-headers)))
    (when copilot-chat-debug
      (message "Fetching models from %s" url))
    (unless quiet
      (message "Fetching available Copilot models..."))
    (request
     url
     :type "GET"
     :headers headers
     :parser
     (apply-partially 'json-parse-buffer
                      :object-type 'alist
                      :false-object
                      :json-false)
     :sync t ; Use synchronous request when called directly
     :complete #'copilot-chat--request-models-cb)))

(defun copilot-chat--request-models-async (&optional quiet)
  "Fetch available models from Copilot API asynchronously.
Optional argument QUIET suppresses user messages when non-nil."
  (let ((url "https://api.githubcopilot.com/models")
        (headers (copilot-chat--get-headers)))
    (when copilot-chat-debug
      (message "Fetching models asynchronously from %s" url))
    (unless quiet
      (message "Fetching available Copilot models in background..."))
    (request
     url
     :type "GET"
     :headers headers
     :parser
     (apply-partially 'json-parse-buffer
                      :object-type 'alist
                      :false-object
                      :json-false)
     :sync nil ; Use asynchronous request for background fetching
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        ;; Process models data
        (let* ((models-vector (alist-get 'data data))
               (models (append models-vector nil)) ; Convert vector to list
               (chat-models nil))
          ;; Filter for chat models
          (dolist (model models)
            (when (and (alist-get 'capabilities model)
                       (equal
                        (alist-get 'type (alist-get 'capabilities model))
                        "chat"))
              (push model chat-models)))

          (when copilot-chat-debug
            (message "Successfully fetched %d models asynchronously"
                     (length chat-models)))

          ;; Store models in instance and cache them
          (let ((sorted-models (nreverse chat-models)))
            (setf (copilot-chat-connection-models copilot-chat--connection)
                  sorted-models)
            (copilot-chat--save-models-to-cache sorted-models)

            ;; Enable policies for models if needed
            (dolist (model sorted-models)
              (when (and (alist-get 'policy model)
                         (equal
                          (alist-get 'state (alist-get 'policy model))
                          "unconfigured"))
                (copilot-chat--request-enable-model-policy
                 (alist-get 'id model))))))))
     :error
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (when copilot-chat-debug
          (message "Error fetching models asynchronously: %S"
                   error-thrown)))))))

(defun copilot-chat--request-cancel (instance)
  "Cancel the current request for INSTANCE."
  (when (fboundp 'copilot-chat--spinner-stop)
    (copilot-chat--spinner-stop instance))
  (let ((backend (copilot-chat--backend instance)))
    (when backend
      (request-abort backend))))


(defun copilot-chat--request-quotas ()
  "Get the current GitHub Copilot quotas for INSTANCE."
  (let ((result '()))
    (request
     "https://api.github.com/rate_limit"
     :type "GET"
     :sync t
     :headers
     `(("content-type" . "application/json")
       ("Accept" . "application/vnd.github+json")
       ("authorization" .
        ,(concat
          "Bearer "
          (copilot-chat-connection-github-token copilot-chat--connection))))
     :parser
     (apply-partially 'json-parse-buffer
                      :object-type 'alist
                      :false-object
                      :json-false)
     :success
     (cl-function
      (lambda (&key data &allow-other-keys)
        (let ((resources (alist-get 'resources data)))
          (dolist (resource resources)
            (let* ((name
                    (capitalize
                     (replace-regexp-in-string
                      "_" " " (symbol-name (car resource)))))
                   (data (cdr resource))
                   (limit (alist-get 'limit data))
                   (used (alist-get 'used data))
                   (remaining (alist-get 'remaining data))
                   (reset (alist-get 'reset data)))
              (push (list name limit used remaining reset) result))))))
     :error
     (cl-function
      (lambda (&key error-thrown &allow-other-keys)
        (message "Error fetching quotas: %S" error-thrown))))
    (nreverse result)))

;; Top-level execute code.
(cl-pushnew
 (make-copilot-chat-backend
  :id 'request
  :init-fn nil
  :clean-fn nil
  :login-fn #'copilot-chat--request-login
  :renew-token-fn #'copilot-chat--request-renew-token
  :ask-fn #'copilot-chat--request-ask
  :cancel-fn #'copilot-chat--request-cancel
  :quotas-fn #'copilot-chat--request-quotas)
 copilot-chat--backend-list
 :test #'equal)

(provide 'copilot-chat-request)
;;; copilot-chat-request.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
