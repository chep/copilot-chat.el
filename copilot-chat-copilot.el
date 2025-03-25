;;; copilot-chat --- copilot-chat-copilot.el  --- copilot chat engine -*- lexical-binding: t;  -*-

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


;;; Code:

(require 'copilot-chat-common)
(require 'copilot-chat-curl)
(require 'copilot-chat-request)

;; customs
(defcustom copilot-chat-prompt-explain "/explain\n"
  "The prompt used by `copilot-chat-explain'."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-review "Please review the following code.\n"
  "The prompt used by `copilot-chat-review'."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-doc "/doc\n"
  "The prompt used by `copilot-chat-doc'."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-fix "/fix\n"
  "The prompt used by `copilot-chat-fix'."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-optimize "/optimize\n"
  "The prompt used by `copilot-chat-optimize'."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-test "/tests\n"
  "The prompt used by `copilot-chat-test'."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-backend 'curl
  "Copilot chat backend.  Can be `curl` or `request`."
  :type 'symbol
  :group 'copilot-chat)

;; constants
(defconst copilot-chat-list-buffer "*Copilot-chat-list"
  "Fixed part of the Copilot chat list buffer name.")


;; Functions
(defun copilot-chat--prompts ()
  "Return assoc list of promts for each command."
  `( (explain . ,copilot-chat-prompt-explain)
     (review . ,copilot-chat-prompt-review)
     (doc . ,copilot-chat-prompt-doc)
     (fix . ,copilot-chat-prompt-fix)
     (optimize . ,copilot-chat-prompt-optimize)
     (test . ,copilot-chat-prompt-test)))

(defun copilot-chat--get-cached-token ()
  "Get the cached GitHub token."
  (let ((token-file (expand-file-name copilot-chat-github-token-file)))
    (when (file-exists-p token-file)
      (with-temp-buffer
        (insert-file-contents token-file)
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun copilot-chat--create (directory)
  "Create a new Copilot chat instance with DIRECTORY as source directory."
  ;; Load models from cache if available
  (let ( (instance (copilot-chat--make
                     :directory directory
                     :model copilot-chat-default-model
                     :chat-buffer nil
                     :first-word-answer t
                     :history nil
                     :buffers nil
                     :prompt-history nil
                     :prompt-history-position nil
                     :yank-index 1
                     :last-yank-start nil
                     :last-yank-end nil
                     :spinner-timer nil
                     :spinner-index 0
                     :spinner-status nil
                     :curl-answer nil
                     :curl-file nil
                     :curl-current-data nil))
         (cached-models (copilot-chat--load-models-from-cache)))
    (when cached-models
      (setf (copilot-chat-connection-models copilot-chat--connection)
        cached-models)
      (message "Loaded models from cache. %d models available." (length cached-models)))

    ;; Schedule background model fetching with slight delay
    (run-with-timer 2 nil #'copilot-chat--fetch-models-async)

    instance))

(defun copilot-chat--fetch-models-async ()
  "Fetch models asynchronously in the background."
  (let ((current-time (round (float-time)))
         (last-fetch-time (copilot-chat-connection-last-models-fetch-time copilot-chat--connection))
         (cooldown-period copilot-chat-models-fetch-cooldown))

    (if (< (- current-time last-fetch-time) cooldown-period)
      (when copilot-chat-debug
        (message "Skipping model fetch - in cooldown period (%d seconds left)"
          (- cooldown-period (- current-time last-fetch-time))))

      (if (not (copilot-chat-connection-github-token copilot-chat--connection))
        (run-with-timer 5 nil #'copilot-chat--fetch-models-async)
        (setf (copilot-chat-connection-last-models-fetch-time copilot-chat--connection) current-time)

        (when copilot-chat-debug
          (message "Starting background model fetch"))

        (condition-case err
          (progn
            (copilot-chat--auth)
            (if (eq copilot-chat-backend 'request)
              (copilot-chat--request-models-async t)
              (copilot-chat--request-models t)))
          (error
            (message "Failed to fetch models in background: %s" (error-message-string err))))))))

(defun copilot-chat--login()
  "Login to GitHub Copilot API."
  (cond
    ((eq copilot-chat-backend 'curl)
      (copilot-chat--curl-login))
    ((eq copilot-chat-backend 'request)
      (copilot-chat--request-login))
    (t
      (error "Unknown backend: %s" copilot-chat-backend))))


(defun copilot-chat--renew-token()
  "Renew the session token."
  (cond
    ((eq copilot-chat-backend 'curl)
      (copilot-chat--curl-renew-token))
    ((eq copilot-chat-backend 'request)
      (copilot-chat--request-renew-token))
    (t
      (error "Unknown backend: %s" copilot-chat-backend))))

(defun copilot-chat--auth()
  "Authenticate with GitHub Copilot API.
We first need github authorization (github token).
Then we need a session token."
  (unless (copilot-chat-connection-github-token copilot-chat--connection)
    (let ((token (copilot-chat--get-cached-token)))
      (if token
        (setf (copilot-chat-connection-github-token copilot-chat--connection) token)
        (copilot-chat--login))))

  (when (null (copilot-chat-connection-token copilot-chat--connection))
    ;; try to load token from ~/.cache/copilot-chat-token
    (let ((token-file (expand-file-name copilot-chat-token-cache)))
      (when (file-exists-p token-file)
        (with-temp-buffer
          (insert-file-contents token-file)
          (setf (copilot-chat-connection-token copilot-chat--connection)
            (json-read-from-string
              (buffer-substring-no-properties (point-min) (point-max))))))))

  (when (or (null (copilot-chat-connection-token copilot-chat--connection))
          (> (round (float-time (current-time)))
            (alist-get 'expires_at (copilot-chat-connection-token copilot-chat--connection))))
    (copilot-chat--renew-token))
  (setf (copilot-chat-connection-ready copilot-chat--connection) t))

(defun copilot-chat--ask (instance prompt callback &optional out-of-context)
  "Ask a question to Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the prompt to send to copilot.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT indicates if prompt is out of context (git commit)."
  (let* ((history (copilot-chat-history instance))
          (new-history (cons (list prompt "user") history)))
    (copilot-chat--auth)
    (cond
      ((eq copilot-chat-backend 'curl)
        (copilot-chat--curl-ask instance prompt callback out-of-context))
      ((eq copilot-chat-backend 'request)
        (copilot-chat--request-ask instance prompt callback out-of-context))
      (t
        (error "Unknown backend: %s" copilot-chat-backend)))
    (unless out-of-context
      (setf (copilot-chat-history instance) new-history))))

(defun copilot-chat--add-buffer (instance buffer)
  "Add a BUFFER to copilot buffers list.
Argument INSTANCE is the copilot chat instance to modify.
Argument BUFFER is the buffer to add to the context."
  (setq buffer (get-buffer buffer))
  (unless (memq buffer (copilot-chat-buffers instance))
    (let* ((buffers (copilot-chat-buffers instance))
            (new-buffers (cons buffer buffers)))
      (setf (copilot-chat-buffers instance) new-buffers))))

(defun copilot-chat--clear-buffers (instance)
  "Remove all buffers in copilot buffers list.
Argument INSTANCE is the copilot chat instance to modify."
  (setf (copilot-chat-buffers instance) nil))

(defun copilot-chat--del-buffer (instance buffer)
  "Remove a BUFFER from copilot buffers list.
Argument INSTANCE is the copilot chat instance to modify.
Argument BUFFER is the buffer to remove from the context."
  (setq buffer (get-buffer buffer))
  (when (memq buffer (copilot-chat-buffers instance))
    (setf (copilot-chat-buffers instance)
      (delete buffer (copilot-chat-buffers instance)))))

(defun copilot-chat--get-buffers (instance)
  "Get copilot buffer list for the given INSTANCE.
Argument INSTANCE is the copilot chat instance to get the buffers for."
  (copilot-chat-buffers instance))

(provide 'copilot-chat-copilot)
;;; copilot-chat-copilot.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; package-lint-main-file: "copilot-chat.el"
;; End:
