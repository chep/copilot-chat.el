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

(require 'copilot-chat-model)
(require 'copilot-chat-backend)
(require 'copilot-chat-frontend)
(require 'copilot-chat-request)
(require 'copilot-chat-prompt-mode)
(require 'org)

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

;; constants
(defconst copilot-chat-quotas-buffer "*Copilot-chat-quotas*"
  "Copilot quotas buffer name.")


;; Functions
(defun copilot-chat--prompts ()
  "Return assoc list of promts for each command."
  `((explain . ,copilot-chat-prompt-explain)
    (review . ,copilot-chat-prompt-review)
    (doc . ,copilot-chat-prompt-doc)
    (fix . ,copilot-chat-prompt-fix)
    (optimize . ,copilot-chat-prompt-optimize)
    (test . ,copilot-chat-prompt-test)))


(defun copilot-chat--write-cached-token (token)
  "Write the GitHub TOKEN to cache."
  (let* ((token-dir
          (file-name-directory
           (expand-file-name copilot-chat-github-token-file)))
         (data
          (let ((ht (make-hash-table :test 'equal)))
            (puthash
             "github.com:Iv1.b507a08c87ecfe98"
             (let ((entry (make-hash-table :test 'equal)))
               (puthash "user" (user-login-name) entry)
               (puthash "oauth_token" token entry)
               (puthash "githubAppId" "Iv1.b507a08c87ecfe98" entry)
               entry)
             ht)
            ht)))
    (when (not (file-directory-p token-dir))
      (make-directory token-dir t))
    (with-temp-file (expand-file-name copilot-chat-github-token-file)
      (insert (json-encode data)))))

(defun copilot-chat--get-cached-token ()
  "Get the cached GitHub token."
  (let ((token-file (expand-file-name copilot-chat-github-token-file)))
    (when (file-exists-p token-file)
      (let* ((json
              (with-temp-buffer
                (insert-file-contents token-file)
                (buffer-string)))
             (data (json-parse-string json))
             (first-key (car (hash-table-keys data)))
             (token-entry (gethash first-key data))
             (oauth-token (gethash "oauth_token" token-entry)))
        oauth-token))))

(defun copilot-chat--create (directory &optional model type)
  "Create a new Copilot chat instance with DIRECTORY as source directory.
Argument DIRECTORY is the directory to use for the instance.
Optional argument MODEL is the model to use for the instance.
Optional argument TYPE is the type of the instance (nil or commit)."
  ;; Load models from cache if available
  (let ((instance
         (copilot-chat--make
          :directory directory
          :model (or model copilot-chat-default-model)
          :type type
          :chat-buffer nil
          :first-word-answer t
          :history nil
          :buffers nil
          :prompt-history-position nil
          :yank-index 1
          :last-yank-start nil
          :last-yank-end nil
          :spinner-timer nil
          :spinner-index 0
          :spinner-status nil))
        (cached-models (copilot-chat--load-models-from-cache)))
    (when cached-models
      (setf (copilot-chat-connection-models copilot-chat--connection)
            cached-models)
      (message "Loaded models from cache. %d models available."
               (length cached-models)))

    ;; Schedule background model fetching with slight delay
    (run-with-timer 2 nil #'copilot-chat--fetch-models-async)

    ;; init backend
    (let ((init-fn (copilot-chat-backend-init-fn (copilot-chat--get-backend))))
      (when init-fn
        (funcall init-fn instance)))

    ;; init frontend
    (let ((init-fn (copilot-chat-frontend-init-fn (copilot-chat--get-frontend)))
          (instance-init-fn
           (copilot-chat-frontend-instance-init-fn
            (copilot-chat--get-frontend))))
      (when (and init-fn (not copilot-chat--frontend-init-p))
        (funcall init-fn)
        (setq copilot-chat--frontend-init-p t))
      (when instance-init-fn
        (funcall instance-init-fn instance)))

    ;; return instance
    instance))

(defun copilot-chat--fetch-models-async ()
  "Fetch models asynchronously in the background."
  (let ((current-time (round (float-time)))
        (last-fetch-time
         (copilot-chat-connection-last-models-fetch-time
          copilot-chat--connection))
        (cooldown-period copilot-chat-models-fetch-cooldown))

    (if (< (- current-time last-fetch-time) cooldown-period)
        (when copilot-chat-debug
          (message "Skipping model fetch - in cooldown period (%d seconds left)"
                   (- cooldown-period (- current-time last-fetch-time))))

      (if (not (copilot-chat-connection-github-token copilot-chat--connection))
          (run-with-timer 5 nil #'copilot-chat--fetch-models-async)
        (setf (copilot-chat-connection-last-models-fetch-time
               copilot-chat--connection)
              current-time)

        (when copilot-chat-debug
          (message "Starting background model fetch"))

        (condition-case err
            (progn
              (copilot-chat--auth)
              (if (eq (copilot-chat--get-backend) 'request)
                  (copilot-chat--request-models-async t)
                (copilot-chat--request-models t)))
          (error
           (message "Failed to fetch models in background: %s"
                    (error-message-string err))))))))

(defun copilot-chat--login ()
  "Login to GitHub Copilot API."
  (let ((login-fn (copilot-chat-backend-login-fn (copilot-chat--get-backend))))
    (if login-fn
        (funcall login-fn)
      (error "No login function for backend: %s" (copilot-chat--get-backend)))))


(defun copilot-chat--renew-token ()
  "Renew the session token."
  (let ((renew-fn
         (copilot-chat-backend-renew-token-fn (copilot-chat--get-backend))))
    (if renew-fn
        (funcall renew-fn)
      (error
       "No renew token function for backend: %s" (copilot-chat--get-backend)))))

(defun copilot-chat--auth ()
  "Authenticate with GitHub Copilot API.
We first need github authorization (github token).
Then we need a session token."
  (unless (copilot-chat-connection-github-token copilot-chat--connection)
    (let ((token (copilot-chat--get-cached-token)))
      (if token
          (setf (copilot-chat-connection-github-token copilot-chat--connection)
                token)
        (copilot-chat--login))))

  (when (null (copilot-chat-connection-token copilot-chat--connection))
    ;; try to load token from ~/.cache/copilot-chat-token
    (let ((token-file (expand-file-name copilot-chat-token-cache)))
      (when (file-exists-p token-file)
        (with-temp-buffer
          (insert-file-contents token-file)
          (let ((token
                 (json-read-from-string
                  (buffer-substring-no-properties (point-min) (point-max)))))
            (if (string= "Bad credentials" (alist-get 'message token))
                (copilot-chat--login)
              (setf (copilot-chat-connection-token copilot-chat--connection)
                    token)))))))

  (when (let* ((token (copilot-chat-connection-token copilot-chat--connection))
               (expires-at (and (listp token) (alist-get 'expires_at token)))
               (now (round (float-time (current-time)))))
          ;; Renew token if missing, malformed, or expired.
          (or (null token)
              (null expires-at)
              (and (numberp expires-at) (> now expires-at))))
    (copilot-chat--renew-token))
  (setf (copilot-chat-connection-ready copilot-chat--connection) t))

(defun copilot-chat--ask (instance prompt callback &optional out-of-context)
  "Ask a question to Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the prompt to send to copilot.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT indicates if prompt is out of context (git commit)."
  (let ((ask-fn (copilot-chat-backend-ask-fn (copilot-chat--get-backend))))
    (copilot-chat--auth)
    (if ask-fn
        (funcall ask-fn instance prompt callback out-of-context)
      (error "No ask function for backend: %s" (copilot-chat--get-backend)))))

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

(defun copilot-chat--display (instance)
  "Internal function to display copilot chat buffer.
Argument INSTANCE is the copilot chat instance to display."
  (let ((base-buffer (copilot-chat--get-buffer instance))
        (window-found nil))
    ;; Check if any window is already displaying the base buffer or an indirect
    ;; buffer
    (cl-block
     window-search
     (dolist (window (window-list))
       (let ((buf (window-buffer window)))
         (when (or (eq buf base-buffer)
                   (eq
                    (with-current-buffer buf
                      (pm-base-buffer))
                    base-buffer))
           (select-window window)
           (switch-to-buffer base-buffer)
           (setq window-found t)
           (cl-return-from window-search)))))
    (unless window-found
      (pop-to-buffer base-buffer))))

(defun copilot-chat--kill-instance (instance)
  "Kill the copilot chat INSTANCE."
  (let* ((buf (copilot-chat--get-buffer instance))
         (lst-buf (copilot-chat--get-list-buffer-create instance))
         (clear-fn
          (copilot-chat-frontend-instance-clean-fn
           (copilot-chat--get-frontend))))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (when (buffer-live-p lst-buf)
      (kill-buffer lst-buf))
    (when clear-fn
      (funcall clear-fn instance))
    (setq copilot-chat--instances (delete instance copilot-chat--instances))))

(defun copilot-chat--create-instance ()
  "Create a new copilot chat instance for a given directory."
  (let* ((current-dir
          (file-name-directory (or (buffer-file-name) default-directory)))
         (directory
          (expand-file-name
           (read-directory-name "Choose a directory: " current-dir)))
         (found (copilot-chat--find-instance directory))
         (instance
          (if found
              found
            (copilot-chat--create directory))))
    (unless found
      (push instance copilot-chat--instances))
    instance))

(defun copilot-chat--find-instance (directory)
  "Find the instance corresponding to a path.
Argument DIRECTORY is the path to search for matching instance."
  (cl-find-if
   (lambda (instance)
     (string-prefix-p (copilot-chat-directory instance) directory))
   copilot-chat--instances))

(defun copilot-chat--ask-for-instance ()
  "Ask for an existing instance or create a new one."
  (if (null copilot-chat--instances)
      (copilot-chat--create-instance)
    (let* ((choice
            (read-multiple-choice
             "Copilot Chat Instance: "
             '((?c "Create new instance" "Create a new Copilot chat instance")
               (?l "Choose from list" "Choose from existing instances"))))
           (key (car choice)))
      (cond
       ((eq key ?l)
        (copilot-chat--choose-instance))
       ((eq key ?c)
        (copilot-chat--create-instance))))))

(defun copilot-chat--current-instance ()
  "Return current instance, create a new one if needed."
  ;; check if we are in a copilot-chat buffer
  (let ((buf (pm-base-buffer)))
    ;; get file corresponding to buf
    ;; if no file, ask for an existing instanceor create a new one
    ;; if an instance as a parent path of the file, use it
    ;; else ask for an existing instance or create a new one
    (let* ((parent
            (expand-file-name
             (file-name-directory
              (or (buffer-file-name buf) default-directory))))
           (existing-instance (copilot-chat--find-instance parent)))
      (if existing-instance
          existing-instance
        (copilot-chat--ask-for-instance)))))

(defun copilot-chat--choose-instance ()
  "Choose an instance from the list of instances."
  ;; create a completion-choices list containing directory of all instances in
  ;; the copilot-chat--instances list. Get directory with
  ;; (copilot-chat-directory instance). Use completing-read to get user choice
  ;; and then use copilot-chat--find-instance to get corresponding instance
  (let* ((choices
          (mapcar
           (lambda (instance)
             (cons (copilot-chat-directory instance) instance))
           copilot-chat--instances))
         (choice
          (completing-read
           "Choose Copilot Chat instance: " (mapcar 'car choices)
           nil t)))
    (copilot-chat--find-instance choice)))

(defun copilot-chat--save-instance (instance file-path)
  "Save the copilot chat INSTANCE to FILE-PATH."
  (let ((temp (copilot-chat--copy instance))
        (save-fn (copilot-chat-frontend-save-fn (copilot-chat--get-frontend))))
    (when save-fn
      (funcall save-fn temp))
    (setf
     (copilot-chat-chat-buffer temp) nil
     (copilot-chat-buffers temp) nil)
    (with-temp-file file-path
      (prin1 temp (current-buffer)))))

(defun copilot-chat--str-to-type (type)
  "Convert TYPE string to symbol."
  (cond
   ((string= type "user")
    'prompt)
   ((string= type "assistant")
    'answer)))


(defun copilot-chat--refill-buffer (instance)
  "Refill the buffer of the copilot chat INSTANCE."
  (with-current-buffer (copilot-chat-chat-buffer instance)
    (let ((inhibit-read-only t)
          (history (reverse (copilot-chat-history instance))))
      (erase-buffer)
      (goto-char (point-min))
      (dolist (entry history)
        (setf (copilot-chat-first-word-answer instance) t)
        (when (and (plist-get entry :content)
                   (not (string= "tool" (plist-get entry :role))))
          (copilot-chat--write-buffer
           instance
           (copilot-chat--format-data
            instance
            (plist-get entry :content)
            (copilot-chat--str-to-type (plist-get entry :role)))
           nil))))))


(defun copilot-chat--load-instance (file-path)
  "Load a copilot chat instance from FILE-PATH."
  (let ((instance
         (with-temp-buffer
           (insert-file-contents file-path)
           (read (current-buffer)))))
    (when (copilot-chat-p instance)
      (let ((existing
             (copilot-chat--find-instance (copilot-chat-directory instance)))
            (load-fn
             (copilot-chat-frontend-load-fn (copilot-chat--get-frontend))))
        (when existing
          (if (y-or-n-p
               (format
                "An instance with directory '%s' already exists.  Replace it? "
                (copilot-chat-directory existing)))
              (copilot-chat--kill-instance existing)
            (cl-return-from
             copilot-chat--load-instance
             (message "Keeping existing instance."))))
        (setf (copilot-chat-file-path instance) file-path)
        (push instance copilot-chat--instances)
        (copilot-chat--display instance)
        (if load-fn
            (funcall load-fn instance)
          (copilot-chat--refill-buffer instance))))))

(defun copilot-chat--quotas ()
  "Display quotas for the copilot chat."
  (let ((quotas-fn
         (copilot-chat-backend-quotas-fn (copilot-chat--get-backend))))
    (when quotas-fn
      (let ((quotas (funcall quotas-fn)))
        (with-current-buffer (get-buffer-create copilot-chat-quotas-buffer)
          (read-only-mode -1)
          (erase-buffer)
          (insert "#+TITLE: Rate Limit Data\n\n")
          (insert
           "| Resource Name       | Limit   | Used   | Remaining | Reset Time           |\n")
          (insert
           "|---------------------+---------+--------+-----------+----------------------|\n")
          (dolist (entry quotas)
            (let ((name (nth 0 entry))
                  (limit (nth 1 entry))
                  (used (nth 2 entry))
                  (remaining (nth 3 entry))
                  (reset
                   (format-time-string "%Y-%m-%d %H:%M:%S"
                                       (seconds-to-time (nth 4 entry)))))
              (insert
               (format "| %-20s | %-7d | %-6d | %-9d | %-20s |\n"
                       name
                       limit
                       used
                       remaining
                       reset))))
          (org-mode)
          (org-table-align)
          (read-only-mode)
          (goto-char (point-min))
          (display-buffer (current-buffer)))))))


(defun copilot-chat--cancel (instance)
  "Cancel the current request in the copilot chat INSTANCE."
  (let ((cancel-fn
         (copilot-chat-backend-cancel-fn (copilot-chat--get-backend))))
    (when cancel-fn
      (funcall cancel-fn instance)
      (message "Request cancelled."))))

(provide 'copilot-chat-copilot)
;;; copilot-chat-copilot.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
