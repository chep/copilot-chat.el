;;; copilot-chat --- copilot-chat-frontend.el --- define copilot frontend interface -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(require 'copilot-chat-common)

(defvar copilot-chat-frontend)

(cl-defstruct copilot-chat-frontend
  id
  init-fn
  clean-fn
  format-fn
  format-code-fn
  create-req-fn
  send-to-buffer-fn
  copy-fn
  yank-fn
  write-fn
  get-buffer-fn
  insert-prompt-fn
  pop-prompt-fn
  goto-input-fn
  get-spinner-buffer-fn)

(defvar copilot-chat--frontend-list '()
  "Copilot-chat frontends and functions list.
Each element must be a `copilot-chat-frontend' struct instance.
Elements are added in the module that defines each front end.")

(cl-declaim (type (list-of copilot-chat-frontend) copilot-chat--frontend-list))

(defun copilot-chat--get-frontend ()
  "Get frontend from custom."
  (cl-find copilot-chat-frontend copilot-chat--frontend-list
    :key #'copilot-chat-frontend-id
    :test #'eq))

(defun copilot-chat--get-buffer(instance)
  "Get Copilot Chat buffer from the active frontend.
Argument INSTANCE is the copilot chat instance to get the buffer for."
  (let ((get-buffer-fn (copilot-chat-frontend-get-buffer-fn
                         (copilot-chat--get-frontend))))
    (when get-buffer-fn
      (funcall get-buffer-fn instance))))

(defun copilot-chat--get-spinner-buffer(instance)
  "Get Spinner buffer from the active frontend.
Argument INSTANCE is the copilot chat instance to get the buffer for."
  (let ((get-buffer-fn (copilot-chat-frontend-get-spinner-buffer-fn
                         (copilot-chat--get-frontend))))
    (when get-buffer-fn
      (funcall get-buffer-fn instance))))

(defun copilot-chat--format-buffer-for-copilot (buffer instance)
  "Format BUFFER content for Copilot with metadata to improve understanding.
INSTANCE is the copilot-chat instance being used."
  (with-current-buffer buffer
    (let* ((file-name (buffer-file-name))
            (relative-path (if file-name
                             (file-relative-name file-name (copilot-chat-directory instance))
                             (buffer-name)))
            (language (if (derived-mode-p 'prog-mode)
                        (replace-regexp-in-string "\\(?:-ts\\)?-mode$" ""
                          (symbol-name major-mode))
                        "text"))
            (content (buffer-substring-no-properties (point-min) (point-max)))
            (line-count (count-lines (point-min) (point-max)))
            (formatted-content ""))

      ;; Add line numbers to content
      (with-temp-buffer
        (insert content)
        (goto-char (point-min))
        (let ((line-num 1))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position))))
              (setq formatted-content
                (concat formatted-content
                  (format "%4d: %s\n" line-num line)))
              (setq line-num (1+ line-num)))
            (forward-line 1))))

      ;; Return the formatted string with metadata
      (format "FILE: %s\nLANGUAGE: %s\nLINES: %d\n\n%s"
        relative-path
        language
        line-count
        formatted-content))))

(defun copilot-chat--create-req (instance prompt no-context)
  "Create a request for Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT Copilot prompt to send.
Argument NO-CONTEXT tells copilot-chat to not send history and buffers.
The create req function is called first and will return new prompt."
  (let ( (create-req-fn (copilot-chat-frontend-create-req-fn
                          (copilot-chat--get-frontend)))
         (messages nil))
    (when create-req-fn
      (setq prompt (funcall create-req-fn prompt no-context)))

    ;; user prompt
    (push (list `(content . ,prompt) `(role . "user")) messages)

    (unless no-context
      ;; history
      (dolist (history (copilot-chat-history instance))
        (push (list `(content . ,(car history)) `(role . ,(cadr history))) messages))
      ;; buffers

      (setf (copilot-chat-buffers instance) (cl-remove-if (lambda (buf) (not (buffer-live-p buf)))
                                              (copilot-chat-buffers instance)))
      (dolist (buffer (copilot-chat-buffers instance))
        (when (buffer-live-p buffer)
          (push (list `(content . ,(copilot-chat--format-buffer-for-copilot buffer instance))
                  `(role . "user"))
            messages))))

    ;; system
    (push (list `(content . ,copilot-chat-prompt) `(role . "system")) messages)


    (json-serialize (if (copilot-chat--model-is-o1 instance)
                      `( (messages . ,(vconcat messages))
                         (model . ,(copilot-chat-model instance))
                         (stream . :false))
                      `( (messages . ,(vconcat messages))
                         (top_p . 1)
                         (model . ,(copilot-chat-model instance))
                         (stream . t)
                         (n . 1)
                         (intent . t)
                         (temperature . 0.1))))))

(provide 'copilot-chat-frontend)
;;; copilot-chat-frontend.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; package-lint-main-file: "copilot-chat.el"
;; End:
