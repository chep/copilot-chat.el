;;; copilot-chat.el --- Copilot chat interface -*- indent-tabs-mode: nil; lisp-indent-offset: 2; lexical-binding: t -*-

;; Copyright (C) 2024  copilot-chat maintainers

;; Author: cedric.chepied <cedric.chepied@gmail.com>
;; Version: 2.0.0
;; URL: https://github.com/chep/copilot-chat.el
;; Package-Requires: ((request "0.3.2") (markdown-mode "2.6") (emacs "27.1") (magit "4.0.0") (transient "0.8.3") (org "9.4.6") (polymode "0.2.2") (shell-maker "0.76.2"))
;; Keywords: convenience, tools


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

;; Add the ability to chat with github copilot

;;; Code:

(require 'copilot-chat-copilot)
(require 'copilot-chat-markdown)
(require 'copilot-chat-org)
(require 'copilot-chat-shell-maker)
(require 'copilot-chat-common)
(require 'copilot-chat-transient)
(require 'copilot-chat-prompts)
(require 'magit)
(require 'cl-lib)

;; customs
(defcustom copilot-chat-list-added-buffers-only nil
  "If non-nil, only show buffers that have been added to the Copilot chat list."
  :type 'boolean
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
Supports glob patterns like '*.lock' or 'node_modules/'."
  :type '(repeat string)
  :group 'copilot-chat)

;; Faces
(defface copilot-chat-list-selected-buffer-face
  '((t :inherit font-lock-keyword-face))
  "Face used for selected buffers in copilot-chat buffer list."
  :group 'copilot-chat)
(defface copilot-chat-list-default-face
  '((t :inherit default))
  "Face used for unselected buffers in copilot-chat buffer list."
  :group 'copilot-chat)

;; Variables
(defvar copilot-chat-list-buffer "*Copilot-chat-list*")
(defvar copilot-chat-prompt-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c RET") 'copilot-chat-prompt-send)
    (define-key map (kbd "C-c C-c") 'copilot-chat-prompt-send)
    (define-key map (kbd "C-c C-q") (lambda()
                                      (interactive)
                                      (bury-buffer)
                                      (delete-window)))
    (define-key map (kbd "C-c C-l") 'copilot-chat-prompt-split-and-list)
    (define-key map (kbd "C-c C-t") 'copilot-chat-transient)
    (define-key map (kbd "M-p") 'copilot-chat-prompt-history-previous)
    (define-key map (kbd "M-n") 'copilot-chat-prompt-history-next)
    map)
  "Keymap for Copilot Chat Prompt mode.")
(defvar copilot-chat-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "SPC") 'copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "C-c C-c") 'copilot-chat-list-clear-buffers)
    (define-key map (kbd "g") 'copilot-chat-list-refresh)
    (define-key map (kbd "q") (lambda()
                                (interactive)
                                (bury-buffer)
                                (delete-window)))
    map)
  "Keymap for `copilot-chat-list-mode'.")
(defvar copilot-chat--prompt-history nil
  "Copilot-chat prompt history.")
(defvar copilot-chat--prompt-history-position nil
  "Current position in copilot-chat prompt history.")

;; Functions
(define-minor-mode copilot-chat-prompt-mode
  "Minor mode for the Copilot Chat Prompt region."
  (use-local-map copilot-chat-prompt-mode-map)
  (run-hooks 'copilot-chat-prompt-mode-hook))

(define-derived-mode copilot-chat-list-mode special-mode "Copilot Chat List"
  "Major mode for listing and managing buffers in Copilot chat."
  (setq buffer-read-only t)
  (copilot-chat-list-refresh))

(defun copilot-chat--write-buffer(data save &optional buffer)
  "Write content to the Copilot Chat BUFFER.
Argument DATA data to be inserted in buffer.
If SAVE is t and BUFFER is nil, save-excursion is called before moving point"
  (if buffer
      (with-current-buffer buffer
        (insert data))
    (with-current-buffer (copilot-chat--get-buffer)
      (let ((write-fn (copilot-chat-frontend-write-fn (copilot-chat--get-frontend))))
        (when write-fn
          (if save
            (save-excursion
              (funcall write-fn data))
            (funcall write-fn data)))))))

(defun copilot-chat--format-data(content _type)
  "Format the CONTENT according to the frontend.
Argument CONTENT is the data to format.
Argument TYPE is the type of data to format: `answer` or `prompt`."
  (let ((format-fn (copilot-chat-frontend-format-fn (copilot-chat--get-frontend))))
    (if format-fn
        (funcall format-fn content _type)
      content)))

(defun copilot-chat-prompt-cb (content &optional buffer)
  "Function called by backend when data is received.
Argument CONTENT is data received from backend.
Optional argument BUFFER is the buffer to write data in."
  (if (string= content copilot-chat--magic)
    (progn
      (when (boundp 'copilot-chat--spinner-timer)
        (copilot-chat--spinner-stop))
      (copilot-chat--write-buffer
        (copilot-chat--format-data "\n\n" 'answer)
        (not copilot-chat-follow)
        buffer))
    (copilot-chat--write-buffer
      (copilot-chat--format-data content 'answer)
      (not copilot-chat-follow)
      buffer)))

(defun copilot-chat--pop-current-prompt()
  "Get current prompt to send and clean it."
  (let ((pop-prompt-fn (copilot-chat-frontend-pop-prompt-fn (copilot-chat--get-frontend))))
    (when pop-prompt-fn
      (funcall pop-prompt-fn))))

(defun copilot-chat-prompt-send ()
  "Function to send the prompt content."
  (interactive)
  (copilot-chat-display)
  (let ((prompt (copilot-chat--pop-current-prompt)))
    (copilot-chat--write-buffer (copilot-chat--format-data prompt 'prompt) nil)
    (push prompt copilot-chat--prompt-history)
    (setq copilot-chat--prompt-history-position nil)
    (copilot-chat--ask prompt 'copilot-chat-prompt-cb)))

;;;###autoload
(defun copilot-chat-ask-and-insert()
  "Send to Copilot a custom prompt and insert answer in current buffer at point."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (let* ((prompt (read-from-minibuffer "Copilot prompt: "))
         (current-buf (current-buffer)))
    (copilot-chat--ask prompt (lambda (content)
                                (copilot-chat-prompt-cb content current-buf)))))

(defun copilot-chat--ask-region(prompt)
  "Send to Copilot a prompt followed by the current selected code.
Argument PROMPT is the prompt to send to Copilot."
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (copilot-chat--insert-and-send-prompt
     (concat (cdr (assoc prompt (copilot-chat--prompts)))
             (copilot-chat--format-code code)))))

;;;###autoload
(defun copilot-chat-explain()
  "Ask Copilot to explain the current selected code."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--ask-region 'explain))

;;;###autoload
(defun copilot-chat-review()
  "Ask Copilot to review the current selected code."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--ask-region 'review))

;;;###autoload
(defun copilot-chat-doc()
  "Ask Copilot to write documentation for the current selected code."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--ask-region 'doc))

;;;###autoload
(defun copilot-chat-fix()
  "Ask Copilot to fix the current selected code."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--ask-region 'fix))

;;;###autoload
(defun copilot-chat-optimize()
  "Ask Copilot to optimize the current selected code."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--ask-region 'optimize))

;;;###autoload
(defun copilot-chat-test()
  "Ask Copilot to generate tests for the current selected code."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--ask-region 'test))

(defun copilot-chat--insert-prompt (prompt)
  "Insert PROMPT in the Copilot Chat prompt region."
  (let ((prompt-fn (copilot-chat-frontend-insert-prompt-fn (copilot-chat--get-frontend))))
    (when prompt-fn
      (funcall prompt-fn prompt))))

(defun copilot-chat--insert-and-send-prompt (prompt)
  "Helper function to prepare buffer and send PROMPT to Copilot."
  (copilot-chat--insert-prompt prompt)
  (copilot-chat-prompt-send))

(defun copilot-chat--get-language ()
  "Get the current language of the buffer."
  (if (derived-mode-p 'prog-mode)  ; current buffer is a programming language buffer
      (let* ((major-mode-str (symbol-name major-mode))
             (lang (replace-regexp-in-string "\\(?:-ts\\)?-mode$" "" major-mode-str)))
        lang)
    nil))

(defun copilot-chat--format-code(code)
  "Format the CODE according to the frontend."
  (let ((format-fn (copilot-chat-frontend-format-code-fn (copilot-chat--get-frontend))))
    (if format-fn
        (funcall format-fn code (copilot-chat--get-language))
      code)))

(defun copilot-chat--custom-prompt-selection()
  "Send to Copilot a custom prompt followed by the current selected code."
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (let* ((prompt (read-from-minibuffer "Copilot prompt: "))
         (code (buffer-substring-no-properties (region-beginning) (region-end)))
         (formatted-prompt (concat prompt "\n" (copilot-chat--format-code code))))
    (copilot-chat--insert-and-send-prompt formatted-prompt)))

;;;###autoload
(defun copilot-chat-explain-symbol-at-line ()
  "Ask Copilot to explain symbol under point, given the code line as background info."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (let* ((symbol (thing-at-point 'symbol))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (prompt (format "Please explain what '%s' means in the context of this code line:\n%s"
                         symbol (copilot-chat--format-code line))))
    (copilot-chat--insert-and-send-prompt prompt)))

;;;###autoload
(defun copilot-chat-explain-defun ()
  "Mark current function definition and ask Copilot to explain it, then unmark."
  (interactive)
  (save-excursion
    (mark-defun)
    (copilot-chat-explain)
    (deactivate-mark)))

;;;###autoload
(defun copilot-chat-custom-prompt-function ()
  "Mark current function and ask copilot-chat with custom prompt."
  (interactive)
  (save-excursion
    (mark-defun)
    (copilot-chat-custom-prompt-selection)
    (deactivate-mark)))

;;;###autoload
(defun copilot-chat-review-whole-buffer ()
  "Mark whole buffer, ask Copilot to review it, then unmark.
It can be used to review the magit diff for my change, or other people's"
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (copilot-chat-review)
    (deactivate-mark)))

;;;###autoload
(defun copilot-chat-switch-to-buffer ()
  "Switch to Copilot Chat buffer, side by side with the current code editing buffer."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (switch-to-buffer-other-window (copilot-chat--get-buffer)))

;;;###autoload
(defun copilot-chat-custom-prompt-selection()
  "Send to Copilot a custom prompt followed by the current selected code."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--custom-prompt-selection))

;;;###autoload
(defun copilot-chat-custom-prompt-mini-buffer ()
  "Read a string with Helm completion, showing historical inputs."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (let* ((prompt "Question for copilot-chat: ")
         (input (read-string prompt nil 'copilot-chat--prompt-history)))
    (copilot-chat--insert-and-send-prompt input)))

;;;###autoload
(defun copilot-chat-list ()
  "Open Copilot Chat list buffer."
  (interactive)
  (let ((buffer (get-buffer-create copilot-chat-list-buffer)))
    (with-current-buffer buffer
      (copilot-chat-list-mode))
    (switch-to-buffer buffer)))

(defun copilot-chat--display ()
  "Internal function to display copilot chat buffer."
  (let ((base-buffer (copilot-chat--get-buffer))
        (window-found nil))

    ;; Check if any window is already displaying the base buffer or an indirect
    ;; buffer
    (cl-block window-search
      (dolist (window (window-list))
        (let ((buf (window-buffer window)))
          (when (or (eq buf base-buffer)
                  (eq (with-current-buffer buf (pm-base-buffer)) base-buffer))
            (select-window window)
            (switch-to-buffer base-buffer)
            (setq window-found t)
            (cl-return-from window-search)))))

    (unless window-found
      (pop-to-buffer base-buffer))))

;;;###autoload
(defun copilot-chat-display ()
  "Display copilot chat buffer."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--display))

;;;###autoload
(defun copilot-chat-hide ()
  "Hide copilot chat buffer."
  (interactive)
  (let ((base-buffer (copilot-chat--get-buffer)))
    (dolist (window (window-list))
      (let ((buf (window-buffer window)))
        (when (or (eq buf base-buffer)
                (eq (with-current-buffer buf (pm-base-buffer)) base-buffer))
          (delete-window window))))))

(defun copilot-chat-add-current-buffer ()
  "Add current buffer in sent buffers list."
  (interactive)
  (copilot-chat--add-buffer (current-buffer))
  (copilot-chat-list-refresh))

(defun copilot-chat-del-current-buffer ()
  "Remove current buffer from sent buffers list."
  (interactive)
  (copilot-chat--del-buffer (current-buffer))
  (copilot-chat-list-refresh))

(defun copilot-chat-add-file (file-path)
  "Add FILE-PATH to copilot-chat buffers without changing current window layout."
  (interactive "fFile to add: ")
  (save-window-excursion
    (let ((current-buf (current-buffer)))
      (find-file file-path)
      (copilot-chat-add-current-buffer)
      (switch-to-buffer current-buf))))

(defun copilot-chat-add-buffers-in-current-window ()
  "Add files in all buffers in the current Emacs window to the Copilot chat."
  (interactive)
  (let ((buffers (mapcar 'window-buffer (window-list)))
        (added-buffers '()))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when buffer-file-name
          (copilot-chat-add-current-buffer)
          (push (buffer-name buffer) added-buffers))))
    (message "Added buffers: %s" (string-join added-buffers ", "))))

(defun copilot-chat-add-files-under-dir ()
  "Add all files with same suffix as current file under current directory.
If there are more than 40 files, refuse to add and show warning message."
  (interactive)
  (if (not buffer-file-name)
      (message "Current buffer is not visiting a file")
    (let* ((current-suffix (file-name-extension buffer-file-name))
           (dir (file-name-directory buffer-file-name))
           (max-files 40)
           (files (directory-files dir t
                                   (concat "\\." current-suffix "$")
                                   t))) ; t means don't include . and ..
      (if (> (length files) max-files)
          (message "Too many files (%d, > %d) found with suffix .%s. Aborting."
                   (length files) max-files current-suffix)
        (dolist (file files)
          (copilot-chat-add-file file))
        (message "Added %d files with suffix .%s"
                 (length files) current-suffix)))))

(defun copilot-chat-list-refresh ()
  "Refresh the list of buffers in the current Copilot chat list buffer."
  (interactive)
  (let* ((pt (point))
         (inhibit-read-only t)
         (buffers (if copilot-chat-list-added-buffers-only
                      (copilot-chat-buffers copilot-chat--instance)
                    (buffer-list)))
         (sorted-buffers (sort buffers
                               (lambda (a b)
                                 (string< (symbol-name (buffer-local-value 'major-mode a))
                                          (symbol-name (buffer-local-value 'major-mode b)))))))
    (with-current-buffer (get-buffer-create copilot-chat-list-buffer)
      (erase-buffer)
      (dolist (buffer sorted-buffers)
        (let ((buffer-name (buffer-name buffer))
              (cop-bufs (copilot-chat--get-buffers)))
          (when (and (not (string-prefix-p " " buffer-name))
                     (not (string-prefix-p "*" buffer-name)))
            (insert (propertize buffer-name
                                'face (if (member buffer cop-bufs)
                                          'copilot-chat-list-selected-buffer-face
                                        'copilot-chat-list-default-face))
                    "\n"))))
      (goto-char pt))))


(defun copilot-chat-list-add-or-remove-buffer ()
  "Add or remove the buffer at point from the Copilot chat list."
  (interactive)
  (let* ((buffer-name (buffer-substring (line-beginning-position) (line-end-position)))
         (buffer (get-buffer buffer-name))
         (cop-bufs (copilot-chat--get-buffers)))
    (when buffer
      (if (member buffer cop-bufs)
          (progn
            (copilot-chat--del-buffer buffer)
            (message "Buffer '%s' removed from Copilot chat list." buffer-name))
        (copilot-chat--add-buffer buffer)
        (message "Buffer '%s' added to Copilot chat list." buffer-name)))
    (copilot-chat-list-refresh)))

(defun copilot-chat-list-clear-buffers ()
  "Clear all buffers from the Copilot chat list."
  (interactive)
  (copilot-chat--clear-buffers)
  (message "Cleared all buffers from Copilot chat list.")
  (copilot-chat-list-refresh))

(defun copilot-chat-prompt-split-and-list()
  "Split prompt window and display buffer list."
  (interactive)
  (let ((split-window-preferred-function nil)
        (split-height-threshold nil)
        (split-width-threshold nil))
    (split-window-right (floor (* 0.8 (window-total-width)))))
  (other-window 1)
  (copilot-chat-list))

(defun copilot-chat-prompt-history-previous()
  "Insert previous prompt in prompt buffer."
  (interactive)
  (let ((prompt (if (null copilot-chat--prompt-history)
                    nil
                  (if (null copilot-chat--prompt-history-position)
                      (progn
                        (setq copilot-chat--prompt-history-position 0)
                        (car copilot-chat--prompt-history))
                    (if (= copilot-chat--prompt-history-position (1- (length copilot-chat--prompt-history)))
                        (car (last copilot-chat--prompt-history))
                      (setq copilot-chat--prompt-history-position (1+ copilot-chat--prompt-history-position))
                      (nth copilot-chat--prompt-history-position copilot-chat--prompt-history))))))
    (when prompt
      (copilot-chat--insert-prompt prompt))))


(defun copilot-chat-prompt-history-next()
  "Insert next prompt in prompt buffer."
  (interactive)
  (let ((prompt (if (null copilot-chat--prompt-history)
                    nil
                  (if (null copilot-chat--prompt-history-position)
                      nil
                    (if (= 0 copilot-chat--prompt-history-position)
                        ""
                      (progn
                        (setq copilot-chat--prompt-history-position (1- copilot-chat--prompt-history-position))
                        (nth copilot-chat--prompt-history-position copilot-chat--prompt-history)))))))
    (when prompt
      (copilot-chat--insert-prompt prompt))))

(defun copilot-chat-reset()
  "Reset copilot chat session."
  (interactive)
  (copilot-chat-list-clear-buffers)
  (copilot-chat--clean)
  (let ((buf (copilot-chat--get-buffer)))
    (when (buffer-live-p buf)
      (kill-buffer buf)))
  (let ((init-fn (copilot-chat-frontend-init-fn (copilot-chat--get-frontend))))
    (when init-fn
      (funcall init-fn))
    (copilot-chat--create)))

(defun copilot-chat--clean()
  "Cleaning function."
  (let ((clean-fn (copilot-chat-frontend-clean-fn (copilot-chat--get-frontend))))
    (when clean-fn
      (funcall clean-fn))))


(defun copilot-chat-send-to-buffer()
  "Send the code block at point to buffer.
Replace selection if any."
  (interactive)
  (let ((send-fn (copilot-chat-frontend-send-to-buffer-fn (copilot-chat--get-frontend))))
    (when send-fn
      (funcall send-fn))))

(defun copilot-chat--get-diff ()
  "Get the diff of staged changes in the current git repository.
Returns a string containing the diff, excluding files specified in
`copilot-chat-ignored-commit-files'.  Returns nil if not in a git
repository or if there are no staged changes.

The diff is generated using `magit-git-insert' and excludes files
matching patterns in `copilot-chat-ignored-commit-files', such as
lock files and build artifacts."
  (let ((default-directory (or (magit-toplevel)
                               (user-error "Not inside a Git repository"))))
    (with-temp-buffer
      ;; First get list of staged files
      (magit-git-insert "diff" "--cached" "--name-only")
      (let* ((staged-files (split-string (buffer-string) "\n" t))
             (files-to-include
              (cl-remove-if
               (lambda (file)
                 (cl-some (lambda (pattern)
                            (or (string-match-p (wildcard-to-regexp pattern) file)
                                (and (string-suffix-p "/" pattern)
                                     (string-prefix-p pattern file))))
                          copilot-chat-ignored-commit-files))
               staged-files)))
        (erase-buffer)
        ;; Then get diff only for non-ignored files
        (when files-to-include
          (apply #'magit-git-insert
                 (append '("diff" "--cached")
                         files-to-include)))
        (buffer-string)))))

(defun copilot-chat-goto-input()
  "Go to the input area."
  (interactive)
  (when (equal (pm-base-buffer) (copilot-chat--get-buffer))
    (let ((goto-fn (copilot-chat-frontend-goto-input-fn (copilot-chat--get-frontend))))
      (when goto-fn
        (funcall goto-fn)))))

(defun copilot-chat--debug (category format-string &rest args)
  "Print debug message when `copilot-chat-debug' is enabled.

CATEGORY is a symbol indicating the message category (e.g., 'commit, 'model, 'auth).
FORMAT-STRING is the format string passed to `message'.
ARGS are the arguments to be formatted according to FORMAT-STRING.

The message is prefixed with '[copilot-chat:CATEGORY]' for easy identification.
No message is printed if `copilot-chat-debug' is nil."
  (when copilot-chat-debug
    (unless (symbolp category)
      (signal 'wrong-type-argument (list 'symbolp category)))
    (unless (stringp format-string)
      (signal 'wrong-type-argument (list 'stringp format-string)))
    (let ((formatted-msg (condition-case err
                             (apply #'format format-string args)
                           (error
                            (message "Error formatting debug message: %S" err)
                            (format "Error formatting message with args: %S" args)))))
      (message "[copilot-chat:%s] %s" category formatted-msg))))

;;;###autoload
(defun copilot-chat-insert-commit-message()
  "Generate and insert a commit message using Copilot.
Uses the current staged changes in git to generate an appropriate commit message.
Requires the repository to have staged changes."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))

  ;; Get magit staged diff
  (let* ((diff (copilot-chat--get-diff))
         (prompt (concat copilot-chat-commit-prompt diff))
         (current-buf (current-buffer))
         (start-pos (point))
         (placeholder-pos nil)
         (streaming-in-progress nil)
         (accumulated-content "")
         (error-occurred nil)
         ;; Store the git commit template comments
         (template-comments
          (save-excursion
            (goto-char (point-min))
            (let ((comments ""))
              (while (re-search-forward "^#.*$" nil t)
                (setq comments (concat comments (match-string 0) "\n")))
              comments))))

    ;; Debug messages using structured format
    (copilot-chat--debug 'commit "Starting commit message generation")
    (copilot-chat--debug 'commit "Diff size: %d bytes, Model: %s"
                         (length diff) copilot-chat-model)

    (cond
     ((string-empty-p diff)
      (copilot-chat--debug 'commit "No changes found in staging area")
      (user-error "No staged changes found. Please stage some changes first"))

     (t
      (message "Generating commit message... Please wait.")

      (insert "# [copilot-chat] Working on generating commit message, please wait... ")
      (setq placeholder-pos (point))
      (insert "\n\n")

      (goto-char start-pos)

      (when (fboundp 'copilot-chat--spinner-start)
        (let ((copilot-chat--buffer current-buf))
          (copilot-chat--spinner-start)))

      (setq streaming-in-progress t)

      (defun cleanup-stream ()
        (setq streaming-in-progress nil)
        (when (fboundp 'copilot-chat--spinner-stop)
          (let ((copilot-chat--buffer current-buf))
            (copilot-chat--spinner-stop)))
        (with-current-buffer current-buf
          (save-excursion
            (goto-char start-pos)
            (when (looking-at "# [copilot-chat] Working on generating commit message, please wait... ")
              (delete-region start-pos (+ start-pos (length "# [copilot-chat] Working on generating commit message, please wait... ")))))))

      ;; Ask Copilot with streaming response
      (copilot-chat--ask
       prompt
       (lambda (content)
         (with-current-buffer current-buf
           (save-excursion
             (if (string= content copilot-chat--magic)
                 ;; End of streaming
                 (progn
                   (cleanup-stream)
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
                       (copilot-chat--spinner-set-status "Generating")
                     (message "Generating commit message...")))

                 ;; Handle error messages
                 (when (string-prefix-p "Error:" content)
                   (setq error-occurred t)
                   (message "%s" content))

                 ;; Append the new content to our accumulator
                 (setq accumulated-content (concat accumulated-content content))

                 ;; Remove placeholder text on first content chunk
                 (goto-char start-pos)
                 (when (looking-at "# [copilot-chat] Working on generating commit message, please wait... ")
                   (delete-region start-pos (+ start-pos (length "# [copilot-chat] Working on generating commit message, please wait... "))))

                 ;; Go to where we need to insert/update content
                 (goto-char start-pos)
                 (delete-region start-pos (+ start-pos (length accumulated-content)))
                 (insert accumulated-content)

                 (copilot-chat--debug 'commit "Received chunk: %d chars"
                                      (length content)))))))
       t)))))


(defun copilot-chat--get-model-choices-with-wait ()
  "Get the list of available models for Copilot Chat, waiting for fetch if needed.
If models haven't been fetched yet and no cache exists, wait for the fetch to complete."
  (let ((models (copilot-chat-models copilot-chat--instance)))
    (if models
        ;; Return list of (name . id) pairs from fetched models, sorted by ID
        (sort
         (mapcar (lambda (model)
                   (let* ((id (alist-get 'id model))
                          (name (alist-get 'name model))
                          ;; Remove "(Preview)" from name if it already contains it
                          (clean-name (replace-regexp-in-string " (Preview)$" "" name))
                          (vendor (alist-get 'vendor model))
                          (capabilities (alist-get 'capabilities model))
                          (limits (alist-get 'limits capabilities))
                          (preview-p (eq t (alist-get 'preview model)))
                          (prompt-tokens (alist-get 'max_prompt_tokens limits))
                          (output-tokens (or (alist-get 'max_output_tokens limits)
                                             (when (string-prefix-p "o1" id) 100000)))
                          (context-tokens (alist-get 'max_context_window_tokens limits)))

                     ;; Format the display string with the new requirements
                     (cons
                      (format "[%s] %s%s (Tokens in/out: %s/%s)" ;
                              vendor
                              clean-name
                              (if preview-p " (Preview)" "")
                              (if prompt-tokens
                                  (format "%dk" (round (/ prompt-tokens 1000)))
                                "?")
                              (if output-tokens
                                  (format "%dk" (round (/ output-tokens 1000)))
                                "?"))
                      id)))
                 models)
         (lambda (a b) (string< (cdr a) (cdr b))))
      ;; No models available - fetch and wait
      (progn
        ;; If not initialized, initialize copilot-chat
        (unless (copilot-chat--ready-p)
          (copilot-chat-reset))

        ;; Try loading from cache first
        (let ((cached-models (copilot-chat--load-models-from-cache)))
          (if cached-models
              (progn
                (setf (copilot-chat-models copilot-chat--instance) cached-models)
                (copilot-chat--get-model-choices-with-wait))
            ;; No cache - need to fetch
            (message "No models available. Fetching from API...")
            (copilot-chat--auth)
            (let ((inhibit-quit t)  ; Prevent C-g during fetch
                  (fetch-done nil))
              (copilot-chat--request-models t)
              ;; Wait for models to be fetched (with timeout)
              (with-timeout (10 (error "Timeout waiting for models to be fetched"))
                (while (not (copilot-chat-models copilot-chat--instance))
                  (sit-for 0.1)))
              (copilot-chat--get-model-choices-with-wait))))))))


;;;###autoload
(defun copilot-chat-set-model (model)
  "Set the Copilot Chat model to MODEL.
Fetches available models from the API if not already fetched."
  (interactive
   (let* ((choices (copilot-chat--get-model-choices-with-wait))
          ;; Create completion list with ID as prefix for unique identification
          (completion-choices (mapcar (lambda (choice)
                                        (let ((name (car choice))
                                              (id (cdr choice)))
                                          (cons (format "[%s] %s" id name) id)))
                                      choices))
          (choice (completing-read "Select Copilot Chat model: "
                                   (mapcar 'car completion-choices)
                                   nil t)))
     ;; Extract model ID from the selected choice
     (let ((model-value (cdr (assoc choice completion-choices))))
       (when copilot-chat-debug
         (message "Setting model to: %s" model-value))
       (list model-value))))

  ;; Check if we need to initialize Copilot chat first
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))

  ;; Set the model value
  (setq copilot-chat-model model)
  (customize-save-variable 'copilot-chat-model copilot-chat-model)
  (message "Copilot Chat model set to %s" copilot-chat-model))

(defun copilot-chat-yank()
  "Insert last code block given by copilot-chat."
  (interactive)
  (setq copilot-chat--yank-index 1
        copilot-chat--last-yank-start nil
        copilot-chat--last-yank-end nil)
  (copilot-chat--yank))

(defun copilot-chat-yank-pop(&optional inc)
  "Replace just-yanked code block with a different block.
INC is the number to use as increment for index in block ring."
  (interactive "*p")
  (if (not (eq last-command 'copilot-chat-yank-pop))
      (unless (eq last-command 'copilot-chat-yank)
        (error "Previous command was not a yank")))
  (if inc
      (setq copilot-chat--yank-index (+ copilot-chat--yank-index inc))
    (setq copilot-chat--yank-index (1+ copilot-chat--yank-index)))
  (copilot-chat--yank)
  (setq this-command 'copilot-chat-yank-pop))

(defun copilot-chat--yank()
  "Insert the code block at the current index in the block ring."
  (let ((yank-fn (copilot-chat-frontend-yank-fn (copilot-chat--get-frontend))))
    (when yank-fn
      (funcall yank-fn))))

;;;###autoload
(defun copilot-chat-clear-auth-cache()
  "Clear the auth cache for Copilot Chat."
  (interactive)
  (copilot-chat-reset)
  ;; remove copilot-chat-token-cache file
  (let ((token-cache-file (expand-file-name copilot-chat-token-cache))
        (github-token-file (expand-file-name copilot-chat-github-token-file)))
    (when (file-exists-p token-cache-file)
      (delete-file token-cache-file))
    (when (file-exists-p github-token-file))
    (delete-file github-token-file))
  (message "Auth cache cleared.")
  (copilot-chat--create))

;;;###autoload
(defun copilot-chat-reset-models()
  "Reset model cache and fetch models again.
This is useful when GitHub adds new models or updates model capabilities.
Clears model cache from memory and disk, then triggers background fetch."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))

  ;; Clear models from instance
  (setf (copilot-chat-models copilot-chat--instance) nil)
  (setf (copilot-chat-last-models-fetch-time copilot-chat--instance) 0)

  ;; Remove cached models file if it exists
  (let ((models-cache-file (expand-file-name copilot-chat-models-cache-file)))
    (when (file-exists-p models-cache-file)
      (delete-file models-cache-file)
      (when copilot-chat-debug
        (message "Removed models cache file: %s" models-cache-file))))

  ;; Trigger a background fetch
  (message "Models cache cleared. Fetching updated models...")
  (copilot-chat--fetch-models-async)

  ;; Return nil for programmatic usage
  nil)

(provide 'copilot-chat)

;;; copilot-chat.el ends here
