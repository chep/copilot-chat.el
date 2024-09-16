;; -*- lexical-binding: t; indent-tabs-mode: nil -*-

;;; copilot-chat.el --- copilot chat interface

;; Copyright (C) 2024  copilot-chat maintainers

;; Author: cedric.chepied <cedric.chepied@gmail.com>
;; Version: 1.1.0
;; URL: https://github.com/chep/copilot-chat.el
;; Package-Requires: ((request) (markdown-mode) (org) (emacs "24.3"))
;; Keywords: github, copilot, chat


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
(require 'copilot-chat-common)

;; customs
(defcustom copilot-chat-frontend 'markdown
  "Frontend to use with `copilot-chat'.  Can be markdown, org or shell-maker."
  :type 'symbol
  :group 'copilot-chat)

;; variables

(defvar copilot-chat-list-buffer "*Copilot-chat-list*")
(defvar copilot-chat-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c q") 'bury-buffer)
    map)
  "Keymap for Copilot Chat major mode.")
(defvar copilot-chat-prompt-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c RET") 'copilot-chat-prompt-send)
    (define-key map (kbd "C-c q") (lambda()
                                    (interactive)
                                    (bury-buffer)
                                    (delete-window)))
    (define-key map (kbd "C-c l") 'copilot-chat-prompt-split-and-list)
    (define-key map (kbd "M-p") 'copilot-chat-prompt-history-previous)
    (define-key map (kbd "M-n") 'copilot-chat-prompt-history-next)
    map)
  "Keymap for Copilot Chat Prompt major mode.")
(defvar copilot-chat-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "SPC") 'copilot-chat-list-add-or-remove-buffer)
    (define-key map (kbd "C-c c") 'copilot-chat-list-clear-buffers)
    (define-key map (kbd "g") 'copilot-chat-list-refresh)
    (define-key map (kbd "q") (lambda()
                                (interactive)
                                (bury-buffer)
                                (delete-window)))
    map)
  "Keymap for `copilot-chat-list-mode'.")
(defvar copilot-chat--prompt-history nil
  "copilot-chat prompt history.")
(defvar copilot-chat--prompt-history-position nil
  "Current position in copilot-chat prompt history")
(defvar copilot-chat--first-word-answer t)
(defvar copilot-chat-frontend-list '((markdown . copilot-chat-markdown-init)
                                     (org . copilot-chat-org-init))
    "Copilot-chat frontend list. Must contain elements like this:
(type . init-function)")


;; functions
(defun copilot-chat-mode ()
  "Major mode for Copilot Chat buffer."
  (interactive)
  (kill-all-local-variables)
  (use-local-map copilot-chat-mode-map)
  (setq major-mode 'copilot-chat-mode)
  (setq mode-name "Copilot Chat")
  (setq buffer-read-only t)
  (run-hooks 'copilot-chat-mode-hook))

(define-derived-mode copilot-chat-mode markdown-view-mode "Copilot Chat"
  "Major mode for the Copilot Chat buffer."
  (read-only-mode 1))

(defun copilot-chat--write-buffer(data &optional buffer)
  "Write content to the Copilot Chat buffer."
  (with-current-buffer (if buffer
                           buffer
                         copilot-chat--buffer)
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
        (insert data))))

(defun copilot-chat--format-data(content type)
    "Format the content according to the frontend."
    content)


(defun copilot-chat-prompt-mode ()
  "Major mode for Copilot Chat Prompt buffer."
  (interactive)
  (kill-all-local-variables)
  (use-local-map copilot-chat-prompt-mode-map)
  (setq major-mode 'copilot-chat-prompt-mode)
  (setq mode-name "Copilot Chat Prompt")
  (run-hooks 'copilot-chat-prompt-mode-hook))

(define-derived-mode copilot-chat-prompt-mode markdown-mode "Copilot Chat Prompt"
  "Major mode for the Copilot Chat Prompt buffer.")

(define-derived-mode copilot-chat-list-mode special-mode "Copilot Chat List"
  "Major mode for listing and managing buffers in Copilot chat."
  (setq buffer-read-only t)
  (copilot-chat-list-refresh))

(defun copilot-chat-prompt-cb (content &optional buffer)
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
  (display-buffer copilot-chat--buffer)
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
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (copilot-chat--prepare-buffers)
    (with-current-buffer copilot-chat--prompt-buffer
      (erase-buffer)
      (insert (concat (cdr (assoc prompt (copilot-chat--prompts))) code)))
    (copilot-chat-prompt-send)))

;;;###autoload
(defun copilot-chat-explain()
  "Ask Copilot to explain the current selected code."
  (interactive)
  (copilot-chat--ask-region 'explain))

;;;###autoload
(defun copilot-chat-review()
  "Ask Copilot to review the current selected code."
  (interactive)
  (copilot-chat--ask-region 'review))

;;;###autoload
(defun copilot-chat-doc()
  "Ask Copilot to write documentation for the current selected code."
  (interactive)
  (copilot-chat--ask-region 'doc))

;;;###autoload
(defun copilot-chat-fix()
  "Ask Copilot to fix the current selected code."
  (interactive)
  (copilot-chat--ask-region 'fix))

;;;###autoload
(defun copilot-chat-optimize()
  "Ask Copilot to optimize the current selected code."
  (interactive)
  (copilot-chat--ask-region 'optimize))

;;;###autoload
(defun copilot-chat-test()
  "Ask Copilot to generate tests for the current selected code."
  (interactive)
  (copilot-chat--ask-region 'test))

;;;###autoload
(defun copilot-chat-custom-prompt-selection()
  "Send to Copilot a custom prompt followed by the current selected code."
  (interactive)
  (let* ((prompt (read-from-minibuffer "Copilot prompt: "))
         (code (buffer-substring-no-properties (region-beginning) (region-end)))
         (formatted-prompt (concat prompt "\n" code)))
    (with-current-buffer copilot-chat--prompt-buffer
      (erase-buffer)
      (insert formatted-prompt))
    (copilot-chat-prompt-send)))



(defun copilot-chat ()
  "Open Copilot Chat buffer."
  (interactive)
  (copilot-chat-reset)
  (let ((buffer copilot-chat--buffer))
    (with-current-buffer buffer
      (copilot-chat-mode))
    (switch-to-buffer buffer)))

(defun copilot-chat-prompt ()
  "Open Copilot Chat Prompt buffer."
  (interactive)
  (let ((buffer copilot-chat--prompt-buffer))
    (with-current-buffer buffer
      (copilot-chat-prompt-mode))
    (switch-to-buffer buffer)))

;;;###autoload
(defun copilot-chat-list ()
  "Open Copilot Chat list buffer."
  (interactive)
  (let ((buffer (get-buffer-create copilot-chat-list-buffer)))
    (with-current-buffer buffer
      (copilot-chat-list-mode))
    (switch-to-buffer buffer)))

(defun copilot-chat--prepare-buffers()
  "Create the copilot-chat--buffer and copilot-chat--prompt-buffer."
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (let ((chat-buffer (get-buffer-create copilot-chat--buffer))
        (prompt-buffer (get-buffer-create copilot-chat--prompt-buffer)))
    (with-current-buffer chat-buffer
      (copilot-chat-mode))
    (with-current-buffer prompt-buffer
      (copilot-chat-prompt-mode))
  (list chat-buffer prompt-buffer)))

;;;###autoload
(defun copilot-chat-display ()
  "Display copilot chat buffers"
  (interactive)
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

(defun copilot-chat-add-current-buffer()
  (interactive)
  (copilot-chat--add-buffer (current-buffer)))

(defun copilot-chat-del-current-buffer()
  (interactive)
  (copilot-chat--del-buffer (current-buffer)))

(defun copilot-chat-list-refresh ()
  "Refresh the list of buffers in the current Copilot chat list buffer."
  (interactive)
  (let ((pt (point))
        (inhibit-read-only t)
        (sorted-buffers (sort (buffer-list)
                              (lambda (a b)
                                (string< (symbol-name (buffer-local-value 'major-mode a))
                                         (symbol-name (buffer-local-value 'major-mode b)))))))
    (erase-buffer)
    (dolist (buffer sorted-buffers)
      (let ((buffer-name (buffer-name buffer))
            (cop-bufs (copilot-chat--get-buffers)))
        (when (and (not (string-prefix-p " " buffer-name))
                   (not (string-prefix-p "*" buffer-name)))
          (insert (propertize buffer-name
                              'face (if (member buffer cop-bufs)
                                        'font-lock-keyword-face
                                      'default))
                  "\n"))))
    (goto-char pt)))


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
  (interactive)
  (let ((split-window-preferred-function nil)
        (split-height-threshold nil)
        (split-width-threshold nil))
    (split-window-right (floor (* 0.8 (window-total-width)))))
  (other-window 1)
  (copilot-chat-list))

(defun copilot-chat-prompt-history-previous()
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
  (interactive)
  (with-current-buffer copilot-chat--prompt-buffer
    (let ((prompt (if (null copilot-chat--prompt-history)
                    nil
                    (if (null copilot-chat--prompt-history-position)
                      nil
                      (if (= 0 copilot-chat--prompt-history-position)
                        ""
                        (setq copilot-chat--prompt-history-position (1- copilot-chat--prompt-history-position))
                        (nth copilot-chat--prompt-history-position copilot-chat--prompt-history))))))
      (when prompt
        (erase-buffer)
        (insert prompt)))))

(defun copilot-chat-reset()
  (interactive)
  (let ((cb (get-buffer copilot-chat--buffer))
        (cpb (get-buffer copilot-chat--prompt-buffer)))
    (when cb
      (kill-buffer cb))
    (when cpb
        (kill-buffer cpb)))
  (copilot-chat--clean)
  (catch 'end
    (dolist (f copilot-chat-frontend-list)
      (when (eq (car f) copilot-chat-frontend)
        (funcall (cdr f))
        (throw 'end nil))))
  (copilot-chat--create))

(defun copilot-chat--clean())

(provide 'copilot-chat)

;;; copilot-chat.el ends here
