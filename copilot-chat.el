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

(defun copilot-chat--write-buffer(data &optional buffer)
  "Write content to the Copilot Chat BUFFER.
Argument DATA data to be inserted in buffer."
  (if buffer
      (with-current-buffer buffer
        (insert data))
    (with-current-buffer (copilot-chat--get-buffer)
	  (let ((write-fn (copilot-chat-frontend-write-fn (copilot-chat--get-frontend))))
        (save-excursion
          (when write-fn
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
      (copilot-chat--write-buffer (copilot-chat--format-data "\n\n" 'answer) buffer)
    (copilot-chat--write-buffer (copilot-chat--format-data content 'answer) buffer)))

(defun copilot-chat--pop-current-prompt()
  "Get current prompt to send and clean it."
  (let ((pop-prompt-fn (copilot-chat-frontend-pop-prompt-fn (copilot-chat--get-frontend))))
    (when pop-prompt-fn
      (funcall pop-prompt-fn))))

(defun copilot-chat-prompt-send ()
  "Function to send the prompt content."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (unless (equal (pm-base-buffer) copilot-chat--buffer)
    (select-window (display-buffer copilot-chat--buffer)))
  (let ((prompt (copilot-chat--pop-current-prompt)))
    (copilot-chat--write-buffer (copilot-chat--format-data prompt 'prompt))
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
  (let* ((prompt "Question for copilot-chat: ")
         (input (read-string prompt nil 'copilot-chat--prompt-history)))
    (copilot-chat--insert-and-send-prompt input)
    ))

;;;###autoload
(defun copilot-chat-list ()
  "Open Copilot Chat list buffer."
  (interactive)
  (let ((buffer (get-buffer-create copilot-chat-list-buffer)))
    (with-current-buffer buffer
      (copilot-chat-list-mode))
    (switch-to-buffer buffer)))

(defun copilot-chat--get-buffer()
  "Create copilot-chat buffers."
  (let ((get-buffer-fn (copilot-chat-frontend-get-buffer-fn (copilot-chat--get-frontend))))
    (when get-buffer-fn
      (funcall get-buffer-fn))))

(defun copilot-chat--display ()
  "Internal function to display copilot chat buffer."
  (switch-to-buffer (copilot-chat--get-buffer)))

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
  (let ((window (get-buffer-window (copilot-chat--get-buffer))))
    (when window
      (delete-window window))))

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
  (let* ((buf (copilot-chat--get-buffer))
         (window (get-buffer-window buf)))
    (when buf
      (kill-buffer buf)
      (when window
        (delete-window window))))
  (copilot-chat--clean)
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
  "Get the diff of all staged files in the current repository and return it as a string."
  (interactive)
  (let ((default-directory (magit-toplevel)))
    (if default-directory
        (with-temp-buffer
          (magit-git-insert "diff" "--cached")
            (buffer-string))
      (message "Not inside a Git repository"))))


;;;###autoload
(defun copilot-chat-insert-commit-message()
  "Insert in the current buffer a copilot generated commit message."
    (interactive)
    (unless (copilot-chat--ready-p)
      (copilot-chat-reset))

    ;; get magit staged diff
    (let* ((diff (copilot-chat--get-diff))
           (prompt (concat copilot-chat-commit-prompt diff))
           (current-buf (current-buffer)))
      (copilot-chat--ask prompt
                         (lambda (content)
                           (with-current-buffer current-buf
                             (if (string= content copilot-chat--magic)
                                 (insert "\n")
                                (insert content))))
                         t)))


(defun copilot-chat--get-model-choices ()
  "Get the list of available models for Copilot Chat."
  (let* ((type (get 'copilot-chat-model 'custom-type))
         (choices (when (eq (car type) 'choice)
                   (cdr type))))
    (let ((mapped-choices
           (mapcar (lambda (choice)
                     (when (eq (car choice) 'const)
                       (cons (plist-get (cdr choice) :tag)
                             (car (last choice))))) ;; Get the string value
                   choices)))
      mapped-choices)))


;;;###autoload
(defun copilot-chat-set-model (model)
  "Set the Copilot Chat model to MODEL."
  (interactive
   (let* ((choices (copilot-chat--get-model-choices))
          (choice (completing-read "Select Copilot Chat model: " (mapcar 'car choices))))
     (let ((model-value (cdr (assoc choice choices))))
       (message "Setting model to: %s" model-value)
       (list model-value))))
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

(provide 'copilot-chat)

;;; copilot-chat.el ends here
