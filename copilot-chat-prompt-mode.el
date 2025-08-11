;;; copilot-chat --- copilot-chat-prompt-mode.el --- copilot chat prompt mode -*- lexical-binding: t; -*-

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

(require 'copilot-chat-common)
(require 'copilot-chat-spinner)

(defvar copilot-chat-prompt-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c RET") 'copilot-chat-prompt-send)
    (define-key map (kbd "C-c C-c") 'copilot-chat-prompt-send)
    (define-key
     map (kbd "C-c C-q")
     (lambda ()
       (interactive)
       (bury-buffer)
       (delete-window)))
    (define-key map (kbd "C-c C-l") 'copilot-chat-prompt-split-and-list)
    (define-key map (kbd "C-c C-t") 'copilot-chat-transient)
    (define-key map (kbd "M-p") 'copilot-chat-prompt-history-previous)
    (define-key map (kbd "M-n") 'copilot-chat-prompt-history-next)
    map)
  "Keymap for Copilot Chat Prompt mode.")
(defvar copilot-chat--prompt-history nil
  "Copilot-chat prompt history.")

(define-minor-mode copilot-chat-prompt-mode
  "Minor mode for the Copilot Chat Prompt region."
  :init-value nil
  :lighter " Copilot Chat Prompt"
  :keymap copilot-chat-prompt-mode-map)

(defun copilot-chat--write-buffer (instance data save &optional buffer)
  "Write content to the Copilot Chat BUFFER.
Argument INSTANCE is the copilot chat instance to use.
Argument DATA data to be inserted in buffer.
If argument SAVE is t and BUFFER nil, `save-excursion' is used.
Optional argument BUFFER is the buffer to write to,
defaults to instance's chat buffer."
  (if buffer
      (with-current-buffer buffer
        (insert data))
    (with-current-buffer (copilot-chat--get-buffer instance)
      (let ((write-fn
             (copilot-chat-frontend-write-fn (copilot-chat--get-frontend))))
        (when write-fn
          (if save
              (save-excursion (funcall write-fn data))
            (funcall write-fn data)))))))

(defun copilot-chat--format-data (instance content type)
  "Format the CONTENT according to the frontend.
Argument INSTANCE is the copilot chat instance to use.
Argument CONTENT is the data to format.
Argument TYPE is the type of data to format: `answer` or `prompt`."
  (let ((format-fn
         (copilot-chat-frontend-format-fn (copilot-chat--get-frontend))))
    (if format-fn
        (funcall format-fn instance content type)
      content)))

(defun copilot-chat-prompt-cb (instance content &optional buffer)
  "Function called by backend when data is received.
Argument INSTANCE is the copilot chat instance to use.
Argument CONTENT is data received from backend.
Optional argument BUFFER is the buffer to write data in."
  (if (string= content copilot-chat--magic)
      (progn
        (when (boundp 'copilot-chat--spinner-timer)
          (copilot-chat--spinner-stop instance))
        (copilot-chat--write-buffer instance
                                    (copilot-chat--format-data
                                     instance "\n\n" 'answer)
                                    (not copilot-chat-follow)
                                    buffer))
    (copilot-chat--write-buffer instance
                                (copilot-chat--format-data
                                 instance content 'answer)
                                (not copilot-chat-follow)
                                buffer)))

(defun copilot-chat--pop-current-prompt (instance)
  "Get current prompt to send and clean it.
Argument INSTANCE is the copilot chat instance to use."
  (let ((pop-prompt-fn
         (copilot-chat-frontend-pop-prompt-fn (copilot-chat--get-frontend))))
    (when pop-prompt-fn
      (funcall pop-prompt-fn instance))))

(provide 'copilot-chat-prompt-mode)
;;; copilot-chat-prompt-mode.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
