;;; copilot-chat --- copilot-chat-common.el --- copilot chat variables and const -*- lexical-binding:t;  -*-

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
;; The shared variables and constants

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

(defun copilot-chat--get-buffer-name ()
  "Get the formatted buffer name including model info."
  (format "*Copilot Chat [%s]*" copilot-chat-model))

(provide 'copilot-chat-common)
;;; copilot-chat-common.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; package-lint-main-file: "copilot-chat.el"
;; End:
