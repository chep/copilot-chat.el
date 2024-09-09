;;; copilot-chat-common.el --- copilot chat variables and const -*- lexical-binding:t; indent-tabs-mode: nil -*-

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
;; All shared variables and constants

;;; Code:

(require 'json)

;; constants
(defconst copilot-chat--magic "#cc#done#!$")


;; structs
(cl-defstruct copilot-chat
  ready
  github-token
  token
  sessionid
  machineid
  authinfo
  history
  buffers
)

;; variables
(defvar copilot-chat--instance
  (make-copilot-chat
   :ready nil
   :github-token nil
   :token nil
   :sessionid nil
   :machineid nil
   :authinfo nil
   :history nil
   :buffers nil
   ))


;; Functions
(defun copilot-chat--uuid ()
  "Generate a UUID."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (random 16384) 16384)
          (logior (random 4096) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun copilot-chat--machine-id ()
  "Generate a machine ID."
  (let ((hex-chars "0123456789abcdef")
        (length 65)
        (hex ""))
    (dotimes (_ length)
      (setq hex (concat hex (string (aref hex-chars (random 16))))))
    hex))

(defun copilot-chat--create-req(prompt)
  "Create a request for Copilot.
Argument PROMPT Copilot prompt to send."
  (let ((messages nil))
    ;; user prompt
    (push (list (cons "content" prompt) (cons "role" "user")) messages)
    ;; history
    (dolist (history (copilot-chat-history copilot-chat--instance))
      (push (list (cons "content" (car history)) (cons "role" (cadr history))) messages))
    ;; buffers
    (setf (copilot-chat-buffers copilot-chat--instance) (cl-remove-if (lambda (buf) (not (buffer-live-p buf)))
                                                                      (copilot-chat-buffers copilot-chat--instance)))
    (dolist (buffer (copilot-chat-buffers copilot-chat--instance))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (push (list (cons "content" (buffer-substring-no-properties (point-min) (point-max))) (cons "role" "user")) messages))))
    ;; system
    (push (list (cons "content" copilot-chat-prompt) (cons "role" "system")) messages)

    (json-encode `(("messages" . ,(vconcat messages))
                   ("top_p" . 1)
                   ("model" . "gpt-4o-2024-05-13")
                   ("stream" . t)
                   ("n" . 1)
                   ("intent" . t)
                   ("temperature" . 0.1)))))


(provide 'copilot-chat-common)
;;; copilot-chat-common.el ends here
