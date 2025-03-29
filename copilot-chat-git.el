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

(require 'copilot-chat-command)
(require 'copilot-chat-copilot)
(require 'copilot-chat-instance)

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
  (let ( (promise (aio-promise))
         (buf (generate-new-buffer " *copilot-chat-shell-command*")))
    (set-process-sentinel
      (apply #'start-process "copilot-chat-shell-command" buf command)
      (lambda (proc _signal)
        (when (memq (process-status proc) '(exit signal))
          (with-current-buffer buf
            (let ((data (buffer-string)))
              (aio-resolve promise
                (lambda () (if (> (length data) 0) (substring data 0 -1) "")))))
          (kill-buffer buf))))
    (aio-await promise)))

(aio-defun copilot-chat--git-root ()
  "Get top folder of current git repo."
  (when (executable-find "git")
    (file-name-directory (aio-await (copilot-chat--exec "git" "rev-parse" "--absolute-git-dir")))))

(aio-defun copilot-chat--get-diff ()
  "Get the diff of staged change in the current git repository.
Returns a string containing the diff, excluding files specified in
`copilot-chat-ignored-commit-files'.  Returns nil if not in a git
repository or if there are no staged changes.

+The diff is generated using git and excludes files matching patterns in
+`copilot-chat-ignored-commit-files', such as lock files and build artifacts."
  (let* ((default-directory (or (aio-await (copilot-chat--git-root))
                              (user-error "Not inside a Git repository")))
          ;; First get list of staged files
          (staged-files (split-string
                          (aio-await
                            (copilot-chat--exec "git" "--no-pager" "diff" "--cached" "--name-only"))
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

;;;###autoload (autoload 'copilot-chat-insert-commit-message-when-ready "copilot-chat" nil t)
(defun copilot-chat-insert-commit-message-when-ready ()
  "Generate and insert a commit message using Copilot.
Uses the current staged changes in git
to generate an appropriate commit message.
Requires the repository to have staged changes."
  (interactive)
  ;; Get magit staged diff
  (aio-with-async
    (let* ((instance (copilot-chat--current-instance))
            (current-buf (current-buffer))
            (start-pos (point))
            (accumulated-content "")
            (error-occurred nil)
            (diff (aio-await (copilot-chat--get-diff)))
            (prompt (concat copilot-chat-commit-prompt diff))
            ;; Store the git commit template comments
            (template-comments
              (save-excursion
                (goto-char (point-min))
                (let ((comments ""))
                  (while (re-search-forward "^#.*$" nil t)
                    (setq comments (concat comments (match-string 0) "\n")))
                  comments)))
            (wait-prompt "# [copilot-chat] Working on generating commit message, please wait... "))

      ;; Debug messages using structured format
      (copilot-chat--debug 'commit "Starting commit message generation")
      (copilot-chat--debug 'commit "Diff size: %d bytes, Model: %s"
        (length diff) (copilot-chat-model instance))

      (cond
        ((string-empty-p diff)
          (copilot-chat--debug 'commit "No changes found in staging area")
          (user-error "No staged changes found.  Please stage some changes first"))

        (t
          (message "Generating commit message... Please wait.")

          (insert wait-prompt)
          (insert "\n\n")

          (goto-char start-pos)

          (when (fboundp 'copilot-chat--spinner-start)
            (copilot-chat--spinner-start instance))

          ;; Ask Copilot with streaming response
          (copilot-chat--ask
            instance
            prompt
            (lambda (instance content)
              (with-current-buffer current-buf
                (save-excursion
                  (if (string= content copilot-chat--magic)
                    ;; End of streaming
                    (progn
                      (when (fboundp 'copilot-chat--spinner-stop)
                        (copilot-chat--spinner-stop instance))
                      (with-current-buffer current-buf
                        (goto-char start-pos)
                        (when (looking-at wait-prompt)
                          (delete-region start-pos (+ start-pos (length wait-prompt)))))
                      (when (not error-occurred)
                        ;; Move point to end of inserted text for convenience
                        (goto-char (+ start-pos (length accumulated-content)))
                        ;; Restore the git commit template comments
                        (save-excursion
                          (goto-char (point-max))
                          (delete-region (point-min) (point-max))
                          (insert accumulated-content "\n\n" template-comments))
                        (copilot-chat--debug 'commit "Generation completed successfully"))
                      (message "Commit message generation completed."))

                    ;; Continue streaming
                    (progn
                      ;; If this is the first response chunk, update the UI accordingly
                      (when (string= accumulated-content "")
                        (if (fboundp 'copilot-chat--spinner-set-status)
                          (copilot-chat--spinner-set-status instance "Generating")
                          (message "Generating commit message...")))

                      ;; Handle error messages
                      (when (string-prefix-p "Error:" content)
                        (setq error-occurred t)
                        (message "%s" content))

                      ;; Append the new content to our accumulator
                      (setq accumulated-content (concat accumulated-content content))

                      ;; Remove placeholder text on first content chunk
                      (goto-char start-pos)
                      (when (looking-at wait-prompt)
                        (delete-region start-pos (+ start-pos (length wait-prompt))))

                      ;; Go to where we need to insert/update content
                      (goto-char start-pos)
                      (delete-region start-pos (min (+ start-pos (length accumulated-content)) (point-max)))
                      (insert accumulated-content)

                      (copilot-chat--debug 'commit "Received chunk: %d chars"
                        (length content)))))))
            t))))))

;;;###autoload (autoload 'copilot-chat-insert-commit-message "copilot-chat" nil t)
(defun copilot-chat-insert-commit-message ()
  "Generate and insert a commit message using Copilot.
Uses the current staged changes in git
to generate an appropriate commit message.
Requires the repository to have staged changes.
This function is expected to be safe to open via magit
when added to `git-commit-setup-hook'."
  (interactive)
  ;;FIXME: I really don't want to do anything delayed by time,
  ;; but I had to in order to make it work anyway.
  ;; In fact, we would like to get rid of this kind of messy control.
  (run-with-timer 1 nil #'copilot-chat-insert-commit-message-when-ready))

(provide 'copilot-chat-git)
;;; copilot-chat-git.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; package-lint-main-file: "copilot-chat.el"
;; End:
