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

(defcustom copilot-chat-commit-model nil
  "The model to use specifically for commit message generation.
When nil, falls back to `copilot-chat-default-model`.
Set via `copilot-chat-set-commit-model'."
  :type '(choice (const :tag "Use default model" nil)
                 (string :tag "Specific model"))
  :group 'copilot-chat)

(defcustom copilot-chat-ignored-commit-files
  '("pnpm-lock.yaml" "package-lock.json" "yarn.lock" "poetry.lock"
    "Cargo.lock" "go.sum" "composer.lock" "Gemfile.lock"
    "requirements.txt" "*.pyc" "*.pyo" "*.pyd"
    "*.so" "*.dylib" "*.dll" "*.exe"
    "*.jar" "*.war" "*.ear"
    "node_modules/" "vendor/" "dist/" "build/")
  "List of file patterns to ignore when generating commit messages.
These are typically large generated files like lock files or build artifacts
that don't need to be included in commit message generation.
Supports glob patterns like `*.lock' or `node_modules/'."
  :type '(repeat string)
  :group 'copilot-chat)

(aio-defun copilot-chat--exec (&rest command)
  "Asynchronously execute command COMMAND and return its output string."
  (let ((promise (aio-promise))
        (buf (generate-new-buffer " *copilot-chat-shell-command*")))
    (set-process-sentinel
     (apply #'start-process "copilot-chat-shell-command" buf command)
     (lambda (proc _signal)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer buf
           (let ((data (buffer-string)))
             (aio-resolve promise
                          (lambda ()
                            (if (> (length data) 0)
                                (substring data 0 -1) "")))))
         (kill-buffer buf))))
    (aio-await promise)))

(aio-defun copilot-chat--git-top-level ()
  "Get top folder of current git repo."
  (when (executable-find "git")
    (let* ((git-dir (aio-await (copilot-chat--exec "git" "rev-parse" "--absolute-git-dir")))
           (default-directory git-dir)
           cdup)
      (cond
       ((file-exists-p "gitdir")      ; worktree case
        (with-temp-buffer
          (insert-file-contents "gitdir")
          (file-name-directory (buffer-string))))
       ((and (setq cdup (aio-await (copilot-chat--exec "git" "rev-parse" "--show-cdup")))
             (not (string-empty-p cdup))) ; submodule case
        cdup)
       (t (file-name-directory git-dir))))))


(aio-defun copilot-chat--git-ls-files (repo-root)
  "Return a list of git managed files in REPO-ROOT.
Uses `git ls-files` to retrieve files that are tracked or not ignored by
Git.  REPO-ROOT must be git top directory."
  (let* ((default-directory repo-root)
         (ls-output
          (aio-await (copilot-chat--exec
                      "git" "--no-pager" "ls-files" "--full-name")))
         (all-files (split-string ls-output "\n" t)))
    (mapcar
     (lambda (file)
       (expand-file-name file repo-root))
     all-files)))

(aio-defun copilot-chat--get-diff ()
  "Get the diff of staged change in the current git repository.
Returns a string containing the diff, excluding files specified in
`copilot-chat-ignored-commit-files'.  Returns nil if not in a git
repository or if there are no staged changes.

+The diff is generated using git and excludes files matching patterns in
+`copilot-chat-ignored-commit-files', such as lock files and build artifacts."
  (let* ((default-directory (or (aio-await (copilot-chat--git-top-level))
                                (user-error "Not inside a Git repository")))
         ;; First get list of staged files
         (staged-files (split-string
                        (aio-await
                         (copilot-chat--exec "git"
                                             "--no-pager"
                                             "diff"
                                             "--cached"
                                             "--name-only"))
                        "\n" t))
         (files-to-include
          (cl-remove-if
           (lambda (file)
             (cl-some (lambda (pattern)
                        (or (string-match-p (wildcard-to-regexp pattern) file)
                            (and (string-suffix-p "/" pattern)
                                 (string-prefix-p pattern file))))
                      copilot-chat-ignored-commit-files))
           staged-files)))
    ;; Then get diff only for non-ignored files
    (when files-to-include
      (aio-await
       (apply #'copilot-chat--exec
              "git" "--no-pager" "diff" "--cached" "--" files-to-include)))))

(defun copilot-chat--create-commit-instance ()
  "Create a temporary, lightweight instance for commit message generation.
This instance is specifically designed for generating commit messages
without creating a chat buffer or setting up a full chat environment."
  (let ((current-dir (file-name-directory (or (buffer-file-name) default-directory))))
    (let ((instance (copilot-chat--make
                     :directory current-dir
                     :model (or copilot-chat-commit-model copilot-chat-default-model)
                     :type 'commit
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
                     :curl-current-data nil)))
      ;; Store instance in our instances list to prevent GC issues with timers
      (push instance copilot-chat--instances)
      instance)))

(defun copilot-chat--get-git-commit-template-comments ()
  "Extract comments (lines starting with #) from the commit buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((comments ""))
      (while (re-search-forward "^#.*$" nil t)
        (setq comments (concat comments (match-string 0) "\n")))
      comments)))

(defun copilot-chat--commit-callback (instance current-buf start-pos accumulated-content template-comments wait-prompt)
  "Callback function for handling commit message generation stream.
INSTANCE is the copilot chat instance receiving data.
CURRENT-BUF is the buffer where commit message will be inserted.
START-POS is the position where commit message insertion begins.
ACCUMULATED-CONTENT stores the progressively built message.
TEMPLATE-COMMENTS contains the original Git commit template comments.
WAIT-PROMPT is the temporary waiting message shown during generation."
  (lambda (_cb-instance content)
    (with-current-buffer current-buf
      (save-excursion
        (if (string= content copilot-chat--magic)
            (progn
              ;; Make sure to stop the spinner timer
              (when (copilot-chat-spinner-timer instance)
                (cancel-timer (copilot-chat-spinner-timer instance))
                (setf (copilot-chat-spinner-timer instance) nil))

              (with-current-buffer current-buf
                (goto-char start-pos)
                (when (looking-at wait-prompt)
                  (delete-region start-pos (+ start-pos (length wait-prompt))))
                ;; Restore the git commit template comments and insert generated message
                (goto-char (point-max))
                (delete-region (point-min) (point-max))
                (insert accumulated-content "\n\n" template-comments))
              (message "Commit message generation completed.")
              (copilot-chat--debug 'commit "Generation completed successfully")

              ;; Remove the temporary instance from instances list to avoid memory leaks
              (setq copilot-chat--instances (delq instance copilot-chat--instances)))

          (progn
            (when (string= accumulated-content "")
              (goto-char start-pos)
              (when (looking-at wait-prompt)
                (delete-region start-pos (+ start-pos (length wait-prompt)))))

            (goto-char start-pos)
            (delete-region start-pos (min (+ start-pos (length accumulated-content)) (point-max)))
            (setq accumulated-content (concat accumulated-content content))
            (insert accumulated-content)
            (copilot-chat--debug 'commit "Received chunk: %d chars" (length content))))))))

;;;###autoload (autoload 'copilot-chat-insert-commit-message-when-ready "copilot-chat" nil t)
(defun copilot-chat-insert-commit-message-when-ready ()
  "Generate and insert a commit message using GitHub Copilot.
Uses the current staged changes in git to generate an appropriate commit
message.  Requires the repository to have staged changes ready for commit."
  (interactive)
  (aio-with-async
    (let* ((instance (copilot-chat--create-commit-instance))
           (current-buf (current-buffer))
           (start-pos (point))
           (diff (aio-await (copilot-chat--get-diff)))
           (template-comments (copilot-chat--get-git-commit-template-comments))
           (wait-prompt (format "# [copilot:%s] Generating commit message..." (copilot-chat-model instance)))
           (accumulated-content ""))

      (copilot-chat--debug 'commit "Starting direct commit message generation")
      (copilot-chat--debug 'commit "Diff size: %d bytes, Model: %s"
                           (length diff) (copilot-chat-model instance))

      (cond
       ((string-empty-p diff)
        (copilot-chat--debug 'commit "No changes found in staging area")
        (setq copilot-chat--instances (delq instance copilot-chat--instances))
        (user-error "No staged changes found.  Please stage some changes first"))

       (t
        (message "Generating commit message...")
        (insert wait-prompt "\n\n")
        (goto-char start-pos)

        (condition-case err
            (copilot-chat--ask
             instance
             (concat copilot-chat-commit-prompt diff)
             (copilot-chat--commit-callback
              instance current-buf start-pos accumulated-content
              template-comments wait-prompt)
             t) ; out-of-context = t
          (error
           ;; Ensure the instance is cleaned up if an error occurs
           (when (copilot-chat-spinner-timer instance)
             (cancel-timer (copilot-chat-spinner-timer instance))
             (setf (copilot-chat-spinner-timer instance) nil))
           (setq copilot-chat--instances (delq instance copilot-chat--instances))
           (signal (car err) (cdr err)))))))))

;;;###autoload (autoload 'copilot-chat-insert-commit-message "copilot-chat" nil t)
(defun copilot-chat-insert-commit-message ()
  "Generate and insert a commit message using Copilot.
Uses the current staged changes in git to generate an appropriate commit
message.  Requires the repository to have staged changes.
This function is expected to be safe to open via magit when added to
`git-commit-setup-hook'."
  (interactive)
  ;;FIXME: I really don't want to do anything delayed by time,
  ;; but I had to in order to make it work anyway.
  ;; In fact, we would like to get rid of this kind of messy control.
  (run-with-timer 1 nil #'copilot-chat-insert-commit-message-when-ready))

;;;###autoload (autoload 'copilot-chat-set-commit-model "copilot-chat" nil t)
(defun copilot-chat-set-commit-model (model)
  "Set the model to use specifically for commit message generation to MODEL.
When called interactively, prompts for available models from the API."
  (interactive
   (let* ((choices (copilot-chat--get-model-choices-with-wait))
          (max-id-width
           (apply #'max
                  (mapcar (lambda (choics)
                            (length (cdr choics))) choices)))
          ;; Create completion list with ID as prefix for unique identification
          (completion-choices
           (mapcar (lambda (choice)
                     (let ((name (car choice))
                           (id (cdr choice)))
                       (cons (format (format "[%%-%ds] %%s" max-id-width)
                                     id name)
                             id)))
                   choices))
          (choice
           (completing-read "Select commit message model: "
                            (mapcar 'car completion-choices)
                            nil t)))
     ;; Extract model ID from the selected choice
     (let ((model-value (cdr (assoc choice completion-choices))))
       (when copilot-chat-debug
         (message "Setting commit model to: %s" model-value))
       (list model-value))))

  ;; Set the commit model value
  (setq copilot-chat-commit-model model)
  (message "Commit message model set to %s" model))

(provide 'copilot-chat-git)
;;; copilot-chat-git.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; package-lint-main-file: "copilot-chat.el"
;; End:
