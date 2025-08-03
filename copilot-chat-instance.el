;;; copilot-chat --- copilot-chat-instance.el --- copilot chat instance management -*- lexical-binding: t; -*-

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

(require 'polymode)

;; GitHub Copilot models: https://api.githubcopilot.com/models
(defcustom copilot-chat-default-model "gpt-4o"
  "The model to use for Copilot chat.
The list of available models will be updated when fetched from the API.
Use `copilot-chat-set-model' to interactively select a model."
  :type 'string
  :group 'copilot-chat)

(cl-defstruct
 (copilot-chat (:constructor copilot-chat--make) (:copier copilot-chat--copy))
 "Struct for Copilot chat state."
 (directory nil :type (or null string))
 (model copilot-chat-default-model :type string)
 (type nil :type (or null symbol))
 (chat-buffer nil :type (or null buffer))
 (first-word-answer t :type boolean)
 (history nil :type list)
 (buffers nil :type list)
 (uses-vision nil :type boolean)
 (prompt-history-position nil :type (or null int))
 (yank-index 1 :type int)
 (last-yank-start nil :type (or null point))
 (last-yank-end nil :type (or null point))
 (spinner-timer nil :type timer)
 (spinner-index 0 :type int)
 (spinner-status nil :type (or null string))
 (-backend nil)
 (-frontend nil)
 (file-path nil :type string)
 (mcp-servers nil))

(defvar copilot-chat--instances (list)
  "Global instance of Copilot chat.")

(cl-declaim (type (list-of copilot-chat) copilot-chat--instances))

(defconst copilot-chat-list-buffer "*Copilot-chat-list"
  "Fixed part of the Copilot chat list buffer name.")

(defun copilot-chat--get-list-buffer-create (instance)
  "Get or create the Copilot chat list buffer for INSTANCE."
  (let ((list-buffer
         (get-buffer-create
          (concat
           copilot-chat-list-buffer
           "-"
           (copilot-chat-directory instance)
           "*"))))
    (with-current-buffer list-buffer
      (setq-local default-directory (copilot-chat-directory instance)))
    list-buffer))

(provide 'copilot-chat-instance)
;;; copilot-chat-instance.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
