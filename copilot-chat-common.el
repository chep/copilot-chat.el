;;; copilot-chat --- copilot-chat-common.el --- copilot chat variables and const -*- indent-tabs-mode: nil; lexical-binding:t; package-lint-main-file: "copilot-chat.el"; -*-

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
(require 'cl-lib)

(require 'copilot-chat-prompts)

;; constants
(defconst copilot-chat--magic "#cc#done#!$")
(defconst copilot-chat--buffer-name "*Copilot Chat*"
  "Name of the Copilot Chat buffer.")

;; customs
(defgroup copilot-chat nil
  "GitHub Copilot chat."
  :group 'tools)

(defcustom copilot-chat-frontend 'org
  "Frontend to use with `copilot-chat'.  Can be org or markdown."
  :type '(choice (const :tag "org-mode" org)
                 (const :tag "markdown" markdown)
                 (const :tag "shell-maker" shell-maker))
  :group 'copilot-chat)

(defcustom copilot-chat-follow nil
  "Follow the chat buffer."
  :type 'boolean
  :group 'copilot-chat)

(defcustom copilot-chat-github-token-file "~/.config/copilot-chat/github-token"
  "The file where to find GitHub token."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-token-cache "~/.cache/copilot-chat/token"
  "The file where the GitHub token is cached."
  :type 'string
  :group 'copilot-chat)

;; Model cache settings
(defcustom copilot-chat-models-cache-file "~/.cache/copilot-chat/models.json"
  "File to cache fetched models."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-models-cache-ttl 86400
  "Time-to-live for cached models in seconds (default: 24 hours)."
  :type 'integer
  :group 'copilot-chat)

(defcustom copilot-chat-models-fetch-cooldown 300
  "Minimum time between model fetch attempts in seconds (default: 5 minutes)."
  :type 'integer
  :group 'copilot-chat)

;; GitHub Copilot models: https://api.githubcopilot.com/models
(defcustom copilot-chat-model "gpt-4o"
  "The model to use for Copilot chat.
The list of available models will be updated when fetched from the API.
Use `copilot-chat-set-model' to interactively select a model."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-model-ignore-picker nil
  "Include models with the `model_picker_enabled' attribute set to `false'.
For most people, a model with this attribute not `true' is useless,
as it is a degraded version or has almost no difference.
Therefore, to reduce noise,
models whose `model_picker_enabled' attribute
is not `true' are not included in the model selection by default."
  :type 'boolean
  :group 'copilot-chat)

(defcustom copilot-chat-debug nil
  "When non-nil, show debug information for API requests."
  :type 'boolean
  :group 'copilot-chat)

;; structs
(cl-defstruct (copilot-chat
               (:constructor copilot-chat--make)
               (:copier nil))
  "Struct for Copilot chat state."
  (ready nil :type boolean)
  (github-token nil :type (or null string))
  (token nil)
  (sessionid nil :type (or null string))
  (machineid nil :type (or null string))
  (history nil :type list)
  (buffers nil :type list)
  (models nil :type list)
  (last-models-fetch-time 0 :type number))

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

;; variables
(defvar copilot-chat--instance
  (copilot-chat--make
   :ready nil
   :github-token nil
   :token nil
   :sessionid nil
   :machineid nil
   :history nil
   :buffers nil
   :models nil
   :last-models-fetch-time 0)
  "Global instance of Copilot chat.")

(defvar copilot-chat--frontend-list
  (list (make-copilot-chat-frontend
         :id 'markdown
         :init-fn #'copilot-chat--markdown-init
         :clean-fn #'copilot-chat--markdown-clean
         :format-fn #'copilot-chat--markdown-format-data
         :format-code-fn #'copilot-chat--markdown-format-code
         :create-req-fn nil
         :send-to-buffer-fn #'copilot-chat--markdown-send-to-buffer
         :copy-fn #'copilot-chat--markdown-copy
         :yank-fn nil
         :write-fn #'copilot-chat--markdown-write
         :get-buffer-fn #'copilot-chat--markdown-get-buffer
         :insert-prompt-fn #'copilot-chat--markdown-insert-prompt
         :pop-prompt-fn #'copilot-chat--markdown-pop-prompt
         :goto-input-fn #'copilot-chat--markdown-goto-input
         :get-spinner-buffer-fn #'copilot-chat--markdown-get-spinner-buffer)
        (make-copilot-chat-frontend
         :id 'org
         :init-fn #'copilot-chat--org-init
         :clean-fn #'copilot-chat--org-clean
         :format-fn #'copilot-chat--org-format-data
         :format-code-fn #'copilot-chat--org-format-code
         :create-req-fn #'copilot-chat--org-create-req
         :send-to-buffer-fn #'copilot-chat--org-send-to-buffer
         :copy-fn #'copilot-chat--org-copy
         :yank-fn #'copilot-chat--org-yank
         :write-fn #'copilot-chat--org-write
         :get-buffer-fn #'copilot-chat--org-get-buffer
         :insert-prompt-fn #'copilot-chat--org-insert-prompt
         :pop-prompt-fn #'copilot-chat--org-pop-prompt
         :goto-input-fn #'copilot-chat--org-goto-input
         :get-spinner-buffer-fn #'copilot-chat--org-get-buffer)
        (make-copilot-chat-frontend
         :id 'shell-maker
         :init-fn #'copilot-chat-shell-maker-init
         :clean-fn #'copilot-chat--shell-maker-clean
         :format-fn nil
         :format-code-fn #'copilot-chat--markdown-format-code
         :create-req-fn nil
         :send-to-buffer-fn nil
         :copy-fn nil
         :yank-fn nil
         :write-fn nil
         :get-buffer-fn #'copilot-chat--shell-maker-get-buffer
         :insert-prompt-fn #'copilot-chat--shell-maker-insert-prompt
         :pop-prompt-fn nil
         :goto-input-fn #'nil
         :get-spinner-buffer-fn #'copilot-chat--shell-maker-get-buffer))
  "Copilot-chat frontends and functions list.")

(defvar copilot-chat--buffer nil)

(defvar copilot-chat--first-word-answer t)

(defvar copilot-chat--yank-index 1
  "Next index to yank.")
(defvar copilot-chat--last-yank-start nil
  "Start position of last yank.")
(defvar copilot-chat--last-yank-end nil
  "End position of last yank.")

;; Functions
(defun copilot-chat--should-fetch-models-p ()
  "Return t if models should be fetched."
  t)

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

(defun copilot-chat--model-is-o1 ()
  "Check if the model is o1."
  (string-prefix-p "o1" copilot-chat-model))

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

(defun copilot-chat--get-frontend ()
  "Get frontend from custom."
  (cl-find copilot-chat-frontend copilot-chat--frontend-list
           :key #'copilot-chat-frontend-id
           :test #'eq))

(defun copilot-chat--get-buffer-name ()
  "Get the formatted buffer name including model info."
  (format "*Copilot Chat [%s]*" copilot-chat-model))

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

(provide 'copilot-chat-common)
;;; copilot-chat-common.el ends here
