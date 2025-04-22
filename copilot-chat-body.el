;;; copilot-chat --- copilot-chat-body.el --- create request body for copilot -*- lexical-binding: t; -*-

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

(require 'copilot-chat-frontend)
(require 'copilot-chat-instance)
(require 'copilot-chat-prompts)

(defcustom copilot-chat-use-copilot-instruction-files t
  "Use custom instructions from `.github/copilot-instructions.md'."
  :type 'boolean
  :group 'copilot-chat)

(defcustom copilot-chat-use-git-commit-instruction-files t
  "Use custom git commit instructions from `.github/git-commit-instructions.md'."
  :type 'boolean
  :group 'copilot-chat)

(defcustom copilot-chat-max-instruction-size 65536
  "Maximum size in bytes of instruction files."
  :type '(choice
          (const :tag "Unlimited" nil)
          integer)
  :group 'copilot-chat)

(defun copilot-chat--read-instruction-file (file-name)
  "Return the content of instruction file FILE-NAME or nil.
If the file is larger than `copilot-chat-max-instruction-size',
ignore it and emit a message."
  (let* ((starting-path (or buffer-file-name default-directory))
         (github-dir (locate-dominating-file starting-path ".github"))
         (instruction-file
          (and github-dir
               (expand-file-name (concat ".github/" file-name) github-dir))))
    (when (and instruction-file (file-readable-p instruction-file))
      ;; Skip the file if it exceeds the configured size limit.
      (when (and copilot-chat-max-instruction-size
                 (> (file-attribute-size (file-attributes instruction-file))
                    copilot-chat-max-instruction-size))
        (message "[copilot-chat] `%s` is larger than %d bytes; ignored."
                 instruction-file copilot-chat-max-instruction-size)
        (cl-return-from copilot-chat--read-instruction-file nil))
      (with-temp-buffer
        (insert-file-contents instruction-file)
        (buffer-string)))))

(defun copilot-chat--read-copilot-instructions-file ()
  "Return the content of `.github/copilot-instructions.md' or nil."
  (when copilot-chat-use-copilot-instruction-files
    (copilot-chat--read-instruction-file "copilot-instructions.md")))

(defun copilot-chat--read-git-commit-instructions-file ()
  "Return the content of `.github/git-commit-instructions.md' or nil."
  (when copilot-chat-use-git-commit-instruction-files
    (copilot-chat--read-instruction-file "git-commit-instructions.md")))

(defun copilot-chat--format-copilot-instructions (instruction-content)
  "Format instruction content according to Copilot's expected format.
INSTRUCTION-CONTENT is the content read from the instructions file."
  (when instruction-content
    (concat "When generating code, please follow these user provided coding instructions. "
            "You can ignore an instruction if it contradicts a system message.\n\n"
            "<instructions>\n"
            instruction-content
            "\n</instructions>")))

(defun copilot-chat--format-buffer-for-copilot (buffer instance)
  "Format BUFFER content for Copilot with metadata to improve understanding.
INSTANCE is the `copilot-chat' instance being used."
  (let ((format-buffer-fn (copilot-chat-frontend-format-buffer-fn
                           (copilot-chat--get-frontend))))
    (if format-buffer-fn
        (funcall format-buffer-fn buffer instance)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun copilot-chat--create-req (instance prompt no-context)
  "Create a request for Copilot.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT Copilot prompt to send.
Argument NO-CONTEXT tells `copilot-chat' to not send history and buffers.
The create req function is called first and will return new prompt."
  (let* ((create-req-fn (copilot-chat-frontend-create-req-fn
                         (copilot-chat--get-frontend)))
         (copilot-instruction-content
          (and copilot-chat-use-copilot-instruction-files
               (copilot-chat--read-copilot-instructions-file)))
         (formatted-copilot-instructions
          (and copilot-instruction-content
               (copilot-chat--format-copilot-instructions copilot-instruction-content)))
         (git-commit-instruction-content
          (and copilot-chat-use-git-commit-instruction-files
               (copilot-chat--read-git-commit-instructions-file)))
         (messages nil))
    ;; Apply create-req-fn if available
    (when create-req-fn
      (setq prompt (funcall create-req-fn prompt no-context)))
    ;; user prompt
    (push (list `(content . ,prompt) `(role . "user")) messages)
    ;; Add context if needed
    (unless no-context
      ;; history
      (dolist (history (copilot-chat-history instance))
        (push (list `(content . ,(car history)) `(role . ,(cadr history))) messages))
      ;; Clean buffer list once and add buffer contents
      (setf (copilot-chat-buffers instance)
            (cl-remove-if-not #'buffer-live-p (copilot-chat-buffers instance)))
      (dolist (buffer (copilot-chat-buffers instance))
        (when (buffer-live-p buffer)
          (push (list `(content . ,(copilot-chat--format-buffer-for-copilot buffer instance))
                      `(role . "user"))
                messages))))
    ;; system.
    ;; Add custom instruction as a separate message if available.
    ;; Prefer Global < Project.
    (when formatted-copilot-instructions
      (push (list `(content . ,formatted-copilot-instructions) `(role . "system")) messages))

    (when (and git-commit-instruction-content (eq (copilot-chat-type instance) 'commit))
      (push (list `(content . ,git-commit-instruction-content) `(role . "system")) messages))

    ;; Global instruction.
    (push (list `(content . ,copilot-chat-prompt) `(role . "system")) messages)

    ;; Create the appropriate payload based on model type
    (json-serialize
     (if (copilot-chat--model-is-o1 instance)
         `((messages . ,(vconcat messages))
           (model . ,(copilot-chat-model instance))
           (stream . :false))
       `((messages . ,(vconcat messages))
         (top_p . 1)
         (model . ,(copilot-chat-model instance))
         (stream . t)
         (n . 1)
         (intent . t)
         (temperature . 0.1))))))

(provide 'copilot-chat-body)
;;; copilot-chat-body.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; package-lint-main-file: "copilot-chat.el"
;; End:
