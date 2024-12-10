;;; copilot-chat.el --- Copilot chat interface -*- indent-tabs-mode: nil; lexical-binding: t -*-

;; Copyright (C) 2024  copilot-chat maintainers

;; Author: cedric.chepied <cedric.chepied@gmail.com>
;; Version: 1.2.0
;; URL: https://github.com/chep/copilot-chat.el
;; Package-Requires: ((request "0.3.2") (markdown-mode "2.6") (emacs "27.1") (chatgpt-shell "1.6.1") (magit "4.0.0"))
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
(require 'copilot-chat-shell-maker)
(require 'copilot-chat-org)
(require 'copilot-chat-common)
(require 'magit)

;; customs
(defcustom copilot-chat-frontend 'markdown
  "Frontend to use with `copilot-chat'.  Can be markdown, org or shell-makerauieuie."
  :type '(choice (const :tag "org-mode" org)
                 (const :tag "markdown" markdown)
                 (const :tag "shell-maker" shell-maker))
  :group 'copilot-chat)


(defcustom copilot-chat-commit-prompt
  "Here is the result of running `git diff --cached`. Please suggest a conventional commit message. Don't add anything else to the response. The following describes conventional commits.
Do not use any markers around the commit message.

# Conventional Commits 1.0.0

## Summary

The Conventional Commits specification is a lightweight convention on top of commit messages.
It provides an easy set of rules for creating an explicit commit history;
which makes it easier to write automated tools on top of.
This convention dovetails with [SemVer](http://semver.org),
by describing the features, fixes, and breaking changes made in commit messages.

The commit message should be structured as follows:

---

```
<type>[optional scope]: <description>

[optional body]

[optional footer(s)]
```
---

<br />
The commit contains the following structural elements, to communicate intent to the
consumers of your library:

1. **fix:** a commit of the _type_ `fix` patches a bug in your codebase (this correlates with [`PATCH`](http://semver.org/#summary) in Semantic Versioning).
1. **feat:** a commit of the _type_ `feat` introduces a new feature to the codebase (this correlates with [`MINOR`](http://semver.org/#summary) in Semantic Versioning).
1. **BREAKING CHANGE:** a commit that has a footer `BREAKING CHANGE:`, or appends a `!` after the type/scope, introduces a breaking API change (correlating with [`MAJOR`](http://semver.org/#summary) in Semantic Versioning).
A BREAKING CHANGE can be part of commits of any _type_.
1. _types_ other than `fix:` and `feat:` are allowed, for example [@commitlint/config-conventional](https://github.com/conventional-changelog/commitlint/tree/master/%40commitlint/config-conventional) (based on the [Angular convention](https://github.com/angular/angular/blob/22b96b9/CONTRIBUTING.md#-commit-message-guidelines)) recommends `build:`, `chore:`,
  `ci:`, `docs:`, `style:`, `refactor:`, `perf:`, `test:`, and others.
1. _footers_ other than `BREAKING CHANGE: <description>` may be provided and follow a convention similar to
  [git trailer format](https://git-scm.com/docs/git-interpret-trailers).

Additional types are not mandated by the Conventional Commits specification, and have no implicit effect in Semantic Versioning (unless they include a BREAKING CHANGE).
<br /><br />
A scope may be provided to a commit's type, to provide additional contextual information and is contained within parenthesis, e.g., `feat(parser): add ability to parse arrays`.


Here is the result of `git diff --cached`:
"
  "The prompt used to generate a commit message."
  :type 'string
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
(defvar copilot-chat-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-q") 'bury-buffer)
    (define-key map (kbd "SPC") 'copilot-chat-custom-prompt-mini-buffer)
    map)
  "Keymap for Copilot Chat major mode.")
(defvar copilot-chat-prompt-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c RET") 'copilot-chat-prompt-send)
    (define-key map (kbd "C-c C-c") 'copilot-chat-prompt-send)
    (define-key map (kbd "C-c C-q") (lambda()
                                    (interactive)
                                    (bury-buffer)
                                    (delete-window)))
    (define-key map (kbd "C-c C-l") 'copilot-chat-prompt-split-and-list)
    (define-key map (kbd "M-p") 'copilot-chat-prompt-history-previous)
    (define-key map (kbd "M-n") 'copilot-chat-prompt-history-next)
    map)
  "Keymap for Copilot Chat Prompt major mode.")
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
(defvar copilot-chat-frontend-list '((markdown . copilot-chat-markdown-init)
                                     (org . copilot-chat-org-init)
                                     (shell-maker . copilot-chat-shell-maker-init))
    "Copilot-chat frontend list.  Must contain elements like this:
\(type . init-function)")


;; Functions
(define-derived-mode copilot-chat-mode markdown-view-mode "Copilot Chat"
  "Major mode for the Copilot Chat buffer."
  (read-only-mode 1)
  (use-local-map copilot-chat-mode-map)
  (setq major-mode 'copilot-chat-mode
        mode-name "Copilot Chat"
        buffer-read-only t)
  (run-hooks 'copilot-chat-mode-hook))

(defun copilot-chat--write-buffer(data &optional buffer)
  "Write content to the Copilot Chat BUFFER.
Argument DATA data to be inserted in buffer."
  (with-current-buffer (if buffer
                           buffer
                         copilot-chat--buffer)
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
        (insert data))))

(defun copilot-chat--format-data(content _type)
    "Format the CONTENT according to the frontend.
Argument CONTENT is the data to format.
Argument TYPE is the type of data to format: `answer` or `prompt`."
    content)


(define-derived-mode copilot-chat-prompt-mode markdown-mode "Copilot Chat Prompt"
  "Major mode for the Copilot Chat Prompt buffer."
  (use-local-map copilot-chat-prompt-mode-map)
  (setq major-mode 'copilot-chat-prompt-mode
        mode-name "Copilot Chat Prompt")
  (run-hooks 'copilot-chat-prompt-mode-hook))

(define-derived-mode copilot-chat-list-mode special-mode "Copilot Chat List"
  "Major mode for listing and managing buffers in Copilot chat."
  (setq buffer-read-only t)
  (copilot-chat-list-refresh))

(defun copilot-chat-prompt-cb (content &optional buffer)
    "Function called by backend when data is received.
Argument CONTENT is data received from backend.
Optional argument BUFFER is the buffer to write data in."
  (if (string= content copilot-chat--magic)
      (copilot-chat--write-buffer (copilot-chat--format-data "\n\n" 'answer) buffer)
    (copilot-chat--write-buffer (copilot-chat--format-data content 'answer) buffer))
  (unless buffer
    (with-current-buffer copilot-chat--buffer
      (goto-char (point-max)))))

(defun copilot-chat-prompt-send ()
  "Function to send the prompt content."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (select-window (display-buffer copilot-chat--buffer))
  (with-current-buffer copilot-chat--prompt-buffer
    (let ((prompt (buffer-substring-no-properties (point-min) (point-max))))
      (erase-buffer)
      (copilot-chat--write-buffer (copilot-chat--format-data prompt 'prompt))
      (push prompt copilot-chat--prompt-history)
      (setq copilot-chat--prompt-history-position nil)
      (copilot-chat--ask prompt 'copilot-chat-prompt-cb))))

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
             code))))

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

(defun copilot-chat--insert-and-send-prompt (prompt)
  "Helper function to prepare buffers and send PROMPT to Copilot.
This function may be overriden by frontend."
  (let* ((prompt-suffix (copilot-chat--build-prompt-suffix))
         (final-prompt (if prompt-suffix
                           (concat prompt "\n" prompt-suffix)
                         prompt)))
    (copilot-chat--prepare-buffers)
    (with-current-buffer copilot-chat--prompt-buffer
      (erase-buffer)
      (insert final-prompt))
    (copilot-chat-prompt-send)))

(defun copilot-chat--build-prompt-suffix ()
    "Build a prompt suffix with the current buffer name."
    (if (derived-mode-p 'prog-mode)  ; current buffer is a programming language buffer
        (let* ((major-mode-str (symbol-name major-mode))
               (lang (replace-regexp-in-string "-mode$" "" major-mode-str))
               (dynamic-suffix (format "current programming language is: %s" lang))
               (suffix (if copilot-chat-prompt-suffix
                           (concat dynamic-suffix ", " copilot-chat-prompt-suffix)
                         dynamic-suffix)))
          suffix)
      copilot-chat-prompt-suffix))

(defun copilot-chat--custom-prompt-selection()
  "Send to Copilot a custom prompt followed by the current selected code.
This function can be overriden by frontend."
  (copilot-chat--prepare-buffers)
  (let* ((prompt (read-from-minibuffer "Copilot prompt: "))
         (code (buffer-substring-no-properties (region-beginning) (region-end)))
         (formatted-prompt (concat prompt "\n" code)))
    (copilot-chat--insert-and-send-prompt formatted-prompt)))

;;;###autoload
(defun copilot-chat-explain-symbol-at-line()
  "Ask Copilot to explain symbol under point, given the code line as background info."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (let* ((symbol (thing-at-point 'symbol))
         (line (buffer-substring-no-properties
                (line-beginning-position)
                (line-end-position)))
         (prompt (format "Please explain what '%s' means in the context of this code line:\n%s"
                         symbol line)))
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
  (switch-to-buffer-other-window copilot-chat--buffer))

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

(defun copilot-chat--prepare-buffers()
  "Create copilot-chat buffers."
  (let ((chat-buffer (get-buffer-create copilot-chat--buffer))
        (prompt-buffer (get-buffer-create copilot-chat--prompt-buffer)))
    (with-current-buffer chat-buffer
      (copilot-chat-mode))
    (with-current-buffer prompt-buffer
      (copilot-chat-prompt-mode))
  (list chat-buffer prompt-buffer)))

(defun copilot-chat--display ()
  "Internal function to display copilot chat buffers.
This can be overrided by frontend."
  (let* ((buffers (copilot-chat--prepare-buffers))
         (chat-buffer (car buffers))
         (prompt-buffer (cadr buffers)))
    (switch-to-buffer chat-buffer)
    (let ((split-window-preferred-function nil)
          (split-height-threshold nil)
          (split-width-threshold nil))
      (split-window-below (floor (* 0.8 (window-total-height)))))
    (other-window 1)
    (switch-to-buffer prompt-buffer)))


;;;###autoload
(defun copilot-chat-display ()
  "Display copilot chat buffers."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (copilot-chat--display))

(defun copilot-chat-add-current-buffer()
  "Add current buffer in sent buffers list."
  (interactive)
  (copilot-chat--add-buffer (current-buffer))
  (copilot-chat-list-refresh))

(defun copilot-chat-del-current-buffer()
  "Remove current buffer from sent buffers list."
  (interactive)
  (copilot-chat--del-buffer (current-buffer))
  (copilot-chat-list-refresh))


(defun copilot-chat-list-refresh ()
  "Refresh the list of buffers in the current Copilot chat list buffer."
  (interactive)
  (let ((pt (point))
        (inhibit-read-only t)
        (sorted-buffers (sort (buffer-list)
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
  (with-current-buffer copilot-chat--prompt-buffer
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
        (erase-buffer)
        (insert prompt)))))


(defun copilot-chat-prompt-history-next()
  "Insert next prompt in prompt buffer."
  (interactive)
  (with-current-buffer copilot-chat--prompt-buffer
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
        (erase-buffer)
        (insert prompt)))))

(defun copilot-chat-reset()
  "Reset copilot chat session."
  (interactive)
  (let ((cb (get-buffer copilot-chat--buffer))
        (cpb (get-buffer copilot-chat--prompt-buffer)))
    (when cb
      (kill-buffer cb))
    (when cpb
      (let ((window (get-buffer-window cpb)))
        (when window
          (delete-window window)))
      (kill-buffer cpb)))
  (copilot-chat--clean)
  (catch 'end
    (dolist (f copilot-chat-frontend-list)
      (when (eq (car f) copilot-chat-frontend)
        (funcall (cdr f))
        (throw 'end nil))))
  (copilot-chat--create))

(defun copilot-chat--clean()
  "Cleaning function for frontends.")

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

(provide 'copilot-chat)

;;; copilot-chat.el ends here
