;;; copilot-chat --- copilot-chat-shell-maker.el --- copilot chat interface, shell-maker frontend -*- indent-tabs-mode: nil; lisp-indent-offset: 2; lexical-binding: t; package-lint-main-file: "copilot-chat.el"; -*-

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

(require 'markdown-mode)
(require 'shell-maker)

(require 'copilot-chat-copilot)
(require 'copilot-chat-prompts)

;; Variables
(defvar copilot-chat--shell-cb-fn nil)
(defvar copilot-chat--shell-config
  (make-shell-maker-config
    :name "Copilot-Chat"
    :execute-command 'copilot-chat--shell-cb))
(defvar copilot-chat--shell-maker-answer-point 0
  "Start of the current answer.")


;; Constants
(defconst copilot-chat--shell-maker-temp-buffer "*copilot-chat-shell-maker-temp*")


;; Functions
(defun copilot-chat--shell-maker-prompt-send()
  "Function to send the prompt content."
  (with-current-buffer (copilot-chat--shell-maker-get-buffer)
    (shell-maker-submit)
    (display-buffer (current-buffer))))

(defun copilot-chat--shell-maker-get-buffer ()
  (unless (buffer-live-p copilot-chat--buffer)
    (setq copilot-chat--buffer (copilot-chat--shell)))
  (let ((tempb (get-buffer-create copilot-chat--shell-maker-temp-buffer))
         (inhibit-read-only t))
    (with-current-buffer tempb
      (markdown-view-mode))
    copilot-chat--buffer))

(defun copilot-chat--shell-maker-font-lock-faces ()
  "Replace faces by font-lock-faces."
  (with-current-buffer copilot-chat--shell-maker-temp-buffer
    (let ((inhibit-read-only t))
      (font-lock-ensure)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((next-change (or (next-property-change (point) nil (point-max)) (point-max)))
               (face (get-text-property (point) 'face)))
          (when face
            (font-lock-append-text-property (point) next-change 'font-lock-face face))
          (goto-char next-change))))))

(defun copilot-chat--shell-maker-copy-faces()
  "Apply faces to the copilot chat buffer."
  (with-current-buffer copilot-chat--shell-maker-temp-buffer
    (save-restriction
      (widen)
      (font-lock-ensure)
      (copilot-chat--shell-maker-font-lock-faces)
      (let ((content (buffer-substring (point-min) (point-max))))
        (with-current-buffer copilot-chat--buffer
          (goto-char (1+ copilot-chat--shell-maker-answer-point))
          (insert content)
          (delete-region (point) (+ (point) (1- (length content))))
          (goto-char (point-max)))))))

(defun copilot-chat--shell-cb-prompt (shell content)
  "Callback for Copilot Chat shell-maker.
Argument SHELL is the shell-maker instance.
Argument CONTENT is copilot chat answer."
  (with-current-buffer copilot-chat--buffer
    (goto-char (point-max))
    (when copilot-chat--first-word-answer
      (setq copilot-chat--first-word-answer nil)
      (let ((str (concat (format-time-string "# [%T] ") (format "Copilot(%s):\n" copilot-chat-model)))
             (inhibit-read-only t))
        (with-current-buffer copilot-chat--shell-maker-temp-buffer
          (insert str))
        (funcall (map-elt shell :write-output) str)))
    (if (string= content copilot-chat--magic)
      (progn
        (funcall (map-elt shell :finish-output) t); the end
        (copilot-chat--shell-maker-copy-faces)
        (setq copilot-chat--first-word-answer t))
      (progn
        (with-current-buffer copilot-chat--shell-maker-temp-buffer
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert content)))
        (funcall (map-elt shell :write-output) content)))))

(defun copilot-chat--shell-cb-prompt-wrapper (shell content)
  "Wrapper around copilot-chat--shell-cb-prompt.
Argument SHELL is the shell-maker instance.
Argument CONTENT is copilot chat answer."
  (if copilot-chat-follow
    (copilot-chat--shell-cb-prompt shell content)
    (save-excursion
      (copilot-chat--shell-cb-prompt shell content))))

(defun copilot-chat--shell-cb (command shell)
  "Callback for Copilot Chat shell-maker.
Argument COMMAND is the command to send to Copilot.
Argument CALLBACK is the callback function to call.
Argument ERROR-CALLBACK is the error callback function to call."
  (setq
    copilot-chat--shell-cb-fn
    (apply-partially #'copilot-chat--shell-cb-prompt-wrapper shell)
    copilot-chat--shell-maker-answer-point (point))
  (let ((inhibit-read-only t))
    (with-current-buffer copilot-chat--shell-maker-temp-buffer
      (erase-buffer)))
  (copilot-chat--ask command copilot-chat--shell-cb-fn))

(defun copilot-chat--shell ()
  "Start a Copilot Chat shell."
  (shell-maker-start
    copilot-chat--shell-config
    t nil t
    (copilot-chat--get-buffer-name)))

(defun copilot-chat--shell-maker-insert-prompt(prompt)
  "Insert PROMPT in the chat buffer."
  (with-current-buffer (copilot-chat--shell-maker-get-buffer)
    (insert prompt)))

(defun copilot-chat--shell-maker-clean()
  "Clean the copilot chat shell-maker frontend."
  (when (buffer-live-p copilot-chat--buffer)
    (with-current-buffer copilot-chat--buffer
      (shell-maker--write-input-ring-history copilot-chat--shell-config)))
  (advice-remove 'copilot-chat-prompt-send #'copilot-chat--shell-maker-prompt-send))

(defun copilot-chat-shell-maker-init()
  "Initialize the copilot chat shell-maker frontend."
  (setq copilot-chat-prompt copilot-chat-markdown-prompt)
  (advice-add 'copilot-chat-prompt-send :override #'copilot-chat--shell-maker-prompt-send))

(provide 'copilot-chat-shell-maker)

;;; copilot-chat-shell-maker.el ends here
