;;; copilot-chat --- copilot-chat-frontend.el --- define copilot frontend interface -*- lexical-binding:t; -*-

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

(defcustom copilot-chat-frontend 'org
  "Frontend to use with `copilot-chat'.  Can be org or markdown."
  :type '(choice (const :tag "org-mode" org)
           (const :tag "markdown" markdown)
           (const :tag "shell-maker" shell-maker))
  :group 'copilot-chat)

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

(defun copilot-chat--get-buffer()
  "Create copilot-chat buffers."
  (let ((get-buffer-fn (copilot-chat-frontend-get-buffer-fn (copilot-chat--get-frontend))))
    (when get-buffer-fn
      (funcall get-buffer-fn))))

(defun copilot-chat--get-spinner-buffer()
  "Create copilot-chat buffers."
  (let ((get-buffer-fn (copilot-chat-frontend-get-spinner-buffer-fn (copilot-chat--get-frontend))))
    (when get-buffer-fn
      (funcall get-buffer-fn))))

(defun copilot-chat--create-req (prompt no-context)
  "Create a request for Copilot.
Argument PROMPT Copilot prompt to send.
Argument NO-CONTEXT tells copilot-chat to not send history and buffers.
The create req function is called first and will return new prompt."
  (let ((create-req-fn (copilot-chat-frontend-create-req-fn (copilot-chat--get-frontend)))
         (messages nil))
    (when create-req-fn
      (setq prompt (funcall create-req-fn prompt no-context)))

    ;; user prompt
    (push (list (cons "content" prompt) (cons "role" "user")) messages)

    (unless no-context
      ;; history
      (dolist (history (copilot-chat-history copilot-chat--instance))
        (push (list (cons "content" (car history)) (cons "role" (cadr history))) messages))
      ;; buffers
      (setf (copilot-chat-buffers copilot-chat--instance) (cl-remove-if (lambda (buf) (not (buffer-live-p buf)))
                                                            (copilot-chat-buffers copilot-chat--instance)))
      (dolist (buffer (copilot-chat-buffers copilot-chat--instance))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (push (list (cons "content" (buffer-substring-no-properties (point-min) (point-max))) (cons "role" "user")) messages)))))

    ;; system
    (push (list (cons "content" copilot-chat-prompt) (cons "role" "system")) messages)


    (json-encode (if (copilot-chat--model-is-o1)
                   `(("messages" . ,(vconcat messages))
                      ("model" . ,copilot-chat-model)
                      ("stream" . :json-false))
                   `(("messages" . ,(vconcat messages))
                      ("top_p" . 1)
                      ("model" . ,copilot-chat-model)
                      ("stream" . t)
                      ("n" . 1)
                      ("intent" . t)
                      ("temperature" . 0.1))))))

(provide 'copilot-chat-frontend)
;;; copilot-chat-frontend.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; package-lint-main-file: "copilot-chat.el"
;; End:
