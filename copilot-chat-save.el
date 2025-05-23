;;; copilot-chat --- copilot-chat-save.el --- copilot chat save management -*- lexical-binding: t; -*-

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

(require 'copilot-chat-instance)

(defun copilot-chat--save-instance (instance file-path)
  "Save the copilot chat INSTANCE to FILE-PATH."
  (let ((temp (copilot-chat--copy instance)))
    (setf
     (copilot-chat-chat-buffer temp) nil
     (copilot-chat-buffers temp) nil)
    (with-temp-file file-path
      (prin1 temp (current-buffer)))))

(defun copilot-chat--str-to-type (type)
  "Convert TYPE string to symbol."
  (cond
   ((string= type "user")
    'prompt)
   ((string= type "assistant")
    'answer)))


(defun copilot-chat--refill-buffer (instance)
  "Refill the buffer of the copilot chat INSTANCE."
  (with-current-buffer (copilot-chat-chat-buffer instance)
    (let ((inhibit-read-only t)
          (history (reverse (copilot-chat-history instance))))
      (erase-buffer)
      (goto-char (point-min))
      (dolist (entry history)
        (setf (copilot-chat-first-word-answer instance) t)
        (copilot-chat--write-buffer
         instance
         (copilot-chat--format-data
          instance (car entry) (copilot-chat--str-to-type (cadr entry)))
         nil)))))


(defun copilot-chat--load-instance (file-path)
  "Load a copilot chat instance from FILE-PATH."
  (let ((instance
         (with-temp-buffer
           (insert-file-contents file-path)
           (read (current-buffer)))))
    (when (copilot-chat-p instance)
      (let ((existing
             (copilot-chat--find-instance (copilot-chat-directory instance))))
        (when existing
          (if (y-or-n-p
               (format
                "An instance with directory '%s' already exists. Replace it? "
                (copilot-chat-directory existing)))
              (copilot-chat--kill-instance existing)
            (cl-return-from
             copilot-chat--load-instance
             (message "Keeping existing instance."))))
        (setf (copilot-chat-file-path instance) file-path)
        (push instance copilot-chat--instances)
        (copilot-chat--display instance)
        (copilot-chat--refill-buffer instance)))))

(provide 'copilot-chat-save)
;;; copilot-chat-save.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
