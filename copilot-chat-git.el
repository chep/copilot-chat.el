;;; copilot-chat --- copilot-chat-git.el --- copilot chat git operation -*- lexical-binding: t; -*-

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

(require 'aio)

(require 'copilot-chat-copilot)
(require 'copilot-chat-frontend)
(require 'copilot-chat-spinner)

(defcustom copilot-chat-commit-model nil
  "The model to use specifically for commit message generation.
When nil, falls back to `copilot-chat-default-model`.
Set via `copilot-chat-set-commit-model'."
  :type '(choice (const :tag "Use default model" nil) (string :tag "Specific model"))
  :group 'copilot-chat)

(defcustom copilot-chat-ignored-commit-files
  '("pnpm-lock.yaml"
    "package-lock.json"
    "yarn.lock"
    "poetry.lock"
    "Cargo.lock"
    "go.sum"
    "composer.lock"
    "Gemfile.lock"
    "requirements.txt"
    "*.pyc"
    "*.pyo"
    "*.pyd"
    "*.so"
    "*.dylib"
    "*.dll"
    "*.exe"
    "*.jar"
    "*.war"
    "*.ear"
    "node_modules/"
    "vendor/"
    "dist/"
    "build/")
  "List of file patterns to ignore when generating commit messages.
These are typically large generated files like lock files or build artifacts
that don't need to be included in commit message generation.
Supports glob patterns like `*.lock' or `node_modules/'."
  :type '(repeat string)
  :group 'copilot-chat)

(defcustom copilot-chat-use-difftastic nil
  "Whether to use difftastic for generating diffs when available.
Difftastic provides syntax-aware diffs that are often more readable.
Requires the `difft` command to be installed.

Note: Difftastic is experimental here.  It is designed for human reviewers;
LLMs may understand standard git diff output better."
  :type 'boolean
  :group 'copilot-chat)

(defvar copilot-chat--git-commit-instance nil
  "Persistent instance for Git commit message generation.")

(aio-defun
 copilot-chat--exec
 (&rest command)
 "Asynchronously execute command COMMAND and return its output string."
 (let ((promise (aio-promise))
       (buf (generate-new-buffer " *copilot-chat-shell-command*")))
   (set-process-sentinel
    (apply #'start-process "copilot-chat-shell-command" buf command)
    (lambda (proc _signal)
      (when (memq (process-status proc) '(exit signal))
        (with-current-buffer buf
          (let ((data (buffer-string)))
            (aio-resolve
             promise
             (lambda ()
               (if (> (length data) 0)
                   (substring data 0 -1)
                 "")))))
        (kill-buffer buf))))
   (aio-await promise)))

(aio-defun
 copilot-chat--git-top-level () "Get top folder of current git repo."
 (when (executable-find "git")
   (let* ((git-dir
           (aio-await
            (copilot-chat--exec "git" "rev-parse" "--absolute-git-dir")))
          (default-directory git-dir)
          cdup)
     (cond
      ((file-exists-p "gitdir")
       (with-temp-buffer
         (insert-file-contents "gitdir")
         (file-name-directory (buffer-string))))
      ((and (setq cdup
                  (aio-await
                   (copilot-chat--exec "git" "rev-parse" "--show-cdup")))
            (not (string-empty-p cdup)))
       cdup)
      (t
       (file-name-directory git-dir))))))


(aio-defun
 copilot-chat--git-ls-files (repo-root)
 "Return a list of git managed files in REPO-ROOT.
Uses `git ls-files` to retrieve files that are tracked or not ignored by
Git.  REPO-ROOT must be git top directory."
 (let* ((default-directory repo-root)
        (ls-output
         (aio-await
          (copilot-chat--exec "git" "--no-pager" "ls-files" "--full-name")))
        (all-files (split-string ls-output "\n" t)))
   (mapcar (lambda (file) (expand-file-name file repo-root)) all-files)))

(defun copilot-chat--format-git-context (status diff)
  "Format git context information for commit message generation.
STATUS is the output of git status command.
DIFF is the output of git diff command."
  (let ((commented-status
         (if (string-empty-p status)
             ""
           (replace-regexp-in-string "^" "# " status))))
    (concat
     "<git_context>\n" "# Git Status Summary:\n" commented-status
     (if (string-empty-p commented-status)
         "\n"
       "\n\n")
     "<git_diff>\n" diff
     (if (or (string-empty-p diff) (string-suffix-p "\n" diff))
         ""
       "\n")
     "</git_diff>\n" "</git_context>")))

(defun copilot-chat--difftastic-available-p ()
  "Check if difftastic is available on the system."
  (and copilot-chat-use-difftastic (executable-find "difft")))

(defun copilot-chat--get-diff-command (files-to-include use-difftastic)
  "Get the appropriate diff command based on configuration.
FILES-TO-INCLUDE is the list of files to include in the diff.
USE-DIFFTASTIC is non-nil to use difftastic if available."
  (if (and use-difftastic (copilot-chat--difftastic-available-p))
      (append
       (list
        "git"
        "-c"
        "diff.external=difft --display=inline --color=never --syntax-highlight=off"
        "--no-pager"
        "diff"
        "--cached"
        "--ext-diff"
        "--no-color"
        "--")
       files-to-include)
    (append
     (list "git" "--no-pager" "diff" "--cached" "--no-color" "--")
     files-to-include)))

(aio-defun
 copilot-chat--get-diff-content ()
 "Get the diff content of staged changes.

Returns a string containing the diff content, formatted by
`copilot-chat--format-git-context`."
 (let* ((default-directory
         (or (aio-await (copilot-chat--git-top-level))
             (user-error "Not inside a Git repository")))
        (staged-files
         (split-string (aio-await
                        (copilot-chat--exec
                         "git" "--no-pager" "diff" "--cached" "--name-only"))
                       "\n" t))
        (files-to-include
         (cl-remove-if
          (lambda (file)
            (cl-some
             (lambda (pattern)
               (or (string-match-p
                    (wildcard-to-regexp pattern) file)
                   (and (string-suffix-p "/" pattern)
                        (string-prefix-p pattern file))))
             copilot-chat-ignored-commit-files))
          staged-files)))
   (when files-to-include
     (let* ((status
             (aio-await
              (copilot-chat--exec
               "git" "status" "--short" "--branch" "--untracked-files=no")))
            (diff-output
             (aio-await
              (apply #'copilot-chat--exec
                     (copilot-chat--get-diff-command
                      files-to-include copilot-chat-use-difftastic)))))
       (copilot-chat--format-git-context status diff-output)))))

(defun copilot-chat--ensure-commit-instance (&optional repo-root)
  "Ensure the commit INSTANCE exists, creating it if necessary.

Optional REPO-ROOT specifies the Git repository's top-level directory."
  (let ((recreate-instance t)
        (instance-dir
         (or repo-root
             (file-name-directory (or (buffer-file-name) default-directory)))))
    (if (and copilot-chat--git-commit-instance
             (copilot-chat-p copilot-chat--git-commit-instance)
             (eq (copilot-chat-type copilot-chat--git-commit-instance) 'commit))
        (if (equal
             (copilot-chat-directory copilot-chat--git-commit-instance)
             instance-dir)
            (setq recreate-instance nil)
          (progn
            (copilot-chat--debug
             'commit
             "Commit instance exists for %s, but current context is %s. Recreating."
             (copilot-chat-directory copilot-chat--git-commit-instance)
             instance-dir)
            (copilot-chat-clear-git-commit-instance)))
      (setq recreate-instance t))

    (when recreate-instance
      (copilot-chat--debug
       'commit
       "Creating/recreating commit instance for directory: %s"
       instance-dir)
      (let ((instance
             (copilot-chat--create
              instance-dir copilot-chat-commit-model 'commit)))
        (setq copilot-chat--git-commit-instance instance)
        (unless (memq instance copilot-chat--instances)
          (push instance copilot-chat--instances))))
    copilot-chat--git-commit-instance))

(defun copilot-chat--get-git-commit-template-comments ()
  "Extract comments (lines starting with #) from the commit buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((comments ""))
      (while (re-search-forward "^#.*$" nil t)
        (setq comments (concat comments (match-string 0) "\n")))
      comments)))

(cl-defstruct
 copilot-chat--commit-callback-params
 "Parameters for commit message generation callback.
All fields are required."
 instance ; The commit instance
 current-buf ; Buffer where commit message will be inserted
 start-pos ; Starting position in current-buf
 accumulated-content ; Accumulated content so far
 template-comments ; Original commit template comments
 wait-prompt ; Temporary prompt shown while generating
 user-prompt-for-this-turn ; The prompt that led to this assistant response
 out-of-context-for-ask) ; Whether copilot-chat--ask was called with out-of-context

(defun copilot-chat--commit-callback (params)
  "Callback function for handling commit message generation stream.
PARAMS is a `copilot-chat--commit-callback-params' struct containing:
- instance: The commit instance
- current-buf: Buffer where the commit message will be inserted
- start-pos: Starting position in current-buf
- accumulated: Content accumulated so far
- template-comments: Original commit template comments
- wait-prompt: Temporary prompt shown while generating
- user-prompt: The prompt that led to this assistant response
- out-of-context: Whether `copilot-chat--ask' was called with out-of-context"
  (lambda (_cb-instance content)
    (with-current-buffer (copilot-chat--commit-callback-params-current-buf
                          params)
      (save-excursion
        (if (string= content copilot-chat--magic)
            (progn
              (copilot-chat--spinner-stop
               (copilot-chat--commit-callback-params-instance params))
              (with-current-buffer
                  (copilot-chat--commit-callback-params-current-buf params)
                (goto-char
                 (copilot-chat--commit-callback-params-start-pos params))
                (when (looking-at
                       (copilot-chat--commit-callback-params-wait-prompt
                        params))
                  (delete-region
                   (copilot-chat--commit-callback-params-start-pos params)
                   (+ (copilot-chat--commit-callback-params-start-pos params)
                      (length
                       (copilot-chat--commit-callback-params-wait-prompt
                        params))))))
              (goto-char (point-max))
              (delete-region (point-min) (point-max))
              (insert
               (copilot-chat--commit-callback-params-accumulated-content params)
               "\n\n"
               (copilot-chat--commit-callback-params-template-comments params))
              (if (copilot-chat--commit-callback-params-out-of-context-for-ask
                   params)
                  (setf
                   (copilot-chat-history
                    (copilot-chat--commit-callback-params-instance params))
                   `((:content
                      ,(copilot-chat--commit-callback-params-accumulated-content
                        params)
                      :role "assistant")
                     (:content
                      ,(copilot-chat--commit-callback-params-user-prompt-for-this-turn
                        params)
                      :role "user")))
                (setf
                 (copilot-chat-history
                  (copilot-chat--commit-callback-params-instance params))
                 (cons
                  `(:content
                    ,(copilot-chat--commit-callback-params-accumulated-content
                      params)
                    :role "assistant")
                  (copilot-chat-history
                   (copilot-chat--commit-callback-params-instance params))))))
          (progn
            (when (string=
                   (copilot-chat--commit-callback-params-accumulated-content
                    params)
                   "")
              (goto-char
               (copilot-chat--commit-callback-params-start-pos params))
              (when (looking-at
                     (copilot-chat--commit-callback-params-wait-prompt params))
                (delete-region
                 (copilot-chat--commit-callback-params-start-pos params)
                 (+ (copilot-chat--commit-callback-params-start-pos params)
                    (length
                     (copilot-chat--commit-callback-params-wait-prompt
                      params))))))
            (goto-char (copilot-chat--commit-callback-params-start-pos params))
            (delete-region
             (copilot-chat--commit-callback-params-start-pos params)
             (min (+ (copilot-chat--commit-callback-params-start-pos params)
                     (length
                      (copilot-chat--commit-callback-params-accumulated-content
                       params)))
                  (point-max)))
            (setf (copilot-chat--commit-callback-params-accumulated-content
                   params)
                  (concat
                   (copilot-chat--commit-callback-params-accumulated-content
                    params)
                   content))
            (insert
             (copilot-chat--commit-callback-params-accumulated-content
              params))))))))

(defmacro copilot-chat--with-commit-context (&rest body)
  "Execute BODY with commit-specific context.
This involves setting `copilot-chat-prompt` to `copilot-chat-commit-prompt`
and temporarily disabling the org frontend's `create-req-fn` if active."
  `(let ((copilot-chat-prompt copilot-chat-commit-prompt)
         (frontend (copilot-chat--get-frontend))
         (original-org-create-req-fn nil))
     (when (and frontend (eq (copilot-chat-frontend-id frontend) 'org))
       (setq original-org-create-req-fn
             (copilot-chat-frontend-create-req-fn frontend))
       (setf (copilot-chat-frontend-create-req-fn frontend) nil)
       (copilot-chat--debug
        'commit "Temporarily disabled org frontend create-req-fn for commit."))
     (unwind-protect
         (progn
           ,@body)
       (when original-org-create-req-fn
         (setf (copilot-chat-frontend-create-req-fn frontend)
               original-org-create-req-fn)))))

;;;###autoload (autoload 'copilot-chat-insert-commit-message-when-ready "copilot-chat" nil t)
(defun copilot-chat-insert-commit-message-when-ready ()
  "Generate and insert a commit message using GitHub Copilot."
  (interactive)
  (when buffer-read-only
    (signal
     'buffer-read-only (format "Buffer `%s' is read-only" (buffer-name))))
  (aio-with-async
   (let* ((instance (copilot-chat--ensure-commit-instance))
          (current-buf (current-buffer))
          (start-pos (point))
          (diff-content (aio-await (copilot-chat--get-diff-content)))
          (template-comments (copilot-chat--get-git-commit-template-comments))
          (wait-prompt
           (format "# [copilot:%s] Generating commit message..."
                   (copilot-chat-model instance)))
          (accumulated-content ""))

     (setf (copilot-chat-history instance) nil)
     (copilot-chat--debug
      'commit "Commit instance history cleared for new generation.")
     (copilot-chat--debug 'commit "Starting initial commit message generation.")
     (copilot-chat--debug
      'commit "Diff content size: %d bytes, Model: %s"
      (if diff-content
          (length diff-content)
        0)
      (copilot-chat-model instance))

     (cond
      ((or (null diff-content) (string-empty-p diff-content))
       (copilot-chat--debug 'commit "No changes found in staging area.")
       (user-error
        "No staged changes found or diff content is empty.  Please stage some changes first"))
      (t
       (message "Generating commit message...")
       (insert wait-prompt "\n\n")
       (goto-char start-pos)
       (copilot-chat--debug
        'commit "User diff content to be sent:\n%s" diff-content)

       (condition-case err
           (copilot-chat--with-commit-context
            (copilot-chat--ask
             instance diff-content
             (copilot-chat--commit-callback
              (make-copilot-chat--commit-callback-params
               :instance instance
               :current-buf current-buf
               :start-pos start-pos
               :accumulated-content accumulated-content
               :template-comments template-comments
               :wait-prompt wait-prompt
               :user-prompt-for-this-turn diff-content
               :out-of-context-for-ask t))
             t))
         (error
          (copilot-chat--spinner-stop instance)
          (signal (car err) (cdr err)))))))))

(defun copilot-chat--retry-async
    (async-func max-retries delay &optional retry-count)
  "Retry ASYNC-FUNC up to MAX-RETRIES times with simple algorithm.
If ASYNC-FUNC fails, it will be retried after DELAY seconds.
RETRY-COUNT is the current retry count, starting from 0.
DELAY is increased on each retry."
  (let ((current-retry (or retry-count 0)))
    (aio-with-async
     (condition-case err
         (aio-await (funcall async-func))
       (error
        (if (< current-retry max-retries)
            (progn
              (message "Retry %d/%d failed: %s"
                       (1+ current-retry)
                       max-retries
                       (error-message-string err))
              ;; `aio-sleep' method don't maybe refresh `(current-buffer)'.
              (run-with-timer
               delay nil #'copilot-chat--retry-async
               async-func
               max-retries
               (* (1+ delay) 2) ; Emacs is local application, so we can use simple backoff instead of exponential
               (1+ current-retry)))
          (signal (car err) (cdr err))))))))

;;;###autoload (autoload 'copilot-chat-insert-commit-message "copilot-chat" nil t)
(defun copilot-chat-insert-commit-message ()
  "Generate and insert a commit message using Copilot.
Uses the current staged changes in git to
generate an appropriate commit message.
Requires the repository to have staged changes.
If error, it will retry after a short delay.
This function is expected to be safe to open via magit when added to
`git-commit-setup-hook'."
  (interactive)
  ;;TODO: I really don't want to do anything delayed by time,
  ;; but I had to in order to make it work anyway.
  ;; In fact, we would like to get rid of this kind of messy control.
  (run-with-timer 1 nil #'copilot-chat--retry-async
                  #'copilot-chat-insert-commit-message-when-ready
                  3
                  1))

;;;###autoload (autoload 'copilot-chat-regenerate-commit-message "copilot-chat" nil t)
(defun copilot-chat-regenerate-commit-message ()
  "Regenerate and insert a new commit message using GitHub Copilot."
  (interactive)
  (aio-with-async
   (let* ((instance (copilot-chat--ensure-commit-instance))
          (current-buf (current-buffer))
          (start-pos (point))
          (template-comments (copilot-chat--get-git-commit-template-comments))
          (additional-instructions "")
          (wait-prompt "")
          (accumulated-content ""))

     (unless (copilot-chat-history instance)
       (message
        "No previous commit message generated in this session. Calling 'copilot-chat-insert-commit-message'.")
       (copilot-chat-insert-commit-message)
       (cl-return-from copilot-chat-regenerate-commit-message))

     (setq
      additional-instructions
      (let
          ((instr
            (read-string
             "Additional instructions for regeneration (or RET for default 'regenerate'): ")))
        (if (string-empty-p instr)
            "Please regenerate a new one, taking the previous attempt and my request into account."
          instr)))

     (setq wait-prompt
           (format "# [copilot:%s] Regenerating commit message..."
                   (copilot-chat-model instance)))
     (copilot-chat--debug 'commit "Starting commit message regeneration.")
     (copilot-chat--debug
      'commit "Additional instructions: %s" additional-instructions)
     (copilot-chat--debug
      'commit
      "Current history for regeneration: %S"
      (copilot-chat-history instance))

     (message "Regenerating commit message...")
     (insert wait-prompt "\n\n")
     (goto-char start-pos)

     (condition-case err
         (copilot-chat--with-commit-context
          (copilot-chat--ask
           instance additional-instructions
           (copilot-chat--commit-callback
            (make-copilot-chat--commit-callback-params
             :instance instance
             :current-buf current-buf
             :start-pos start-pos
             :accumulated-content accumulated-content
             :template-comments template-comments
             :wait-prompt wait-prompt
             :user-prompt-for-this-turn additional-instructions
             :out-of-context-for-ask nil))
           nil))
       (error
        (copilot-chat--spinner-stop instance)
        (signal (car err) (cdr err)))))))

;;;###autoload
(defun copilot-chat-clear-git-commit-instance ()
  "Clear and remove the persistent Git commit instance."
  (interactive)
  (when copilot-chat--git-commit-instance
    (copilot-chat--debug 'commit "Clearing persistent Git commit instance.")
    (setq copilot-chat--instances
          (delq copilot-chat--git-commit-instance copilot-chat--instances))
    (when (copilot-chat-spinner-timer copilot-chat--git-commit-instance)
      (cancel-timer
       (copilot-chat-spinner-timer copilot-chat--git-commit-instance)))
    (setq copilot-chat--git-commit-instance nil)
    (message "Persistent Git commit instance cleared."))
  (unless copilot-chat--git-commit-instance
    (message "No active persistent Git commit instance to clear.")))

(provide 'copilot-chat-git)
;;; copilot-chat-git.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
