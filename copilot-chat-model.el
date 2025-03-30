;;; copilot-chat --- copilot-chat-model.el --- copilot chat model -*- lexical-binding: t; -*-

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
(require 'json)

(require 'copilot-chat-debug)

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

(defcustom copilot-chat-model-ignore-picker nil
  "Include models with the `model_picker_enabled' attribute set to `false'.
For most people, a model with this attribute not `true' is useless,
as it is a degraded version or has almost no difference.
Therefore, to reduce noise,
models whose `model_picker_enabled' attribute
is not `true' are not included in the model selection by default."
  :type 'boolean
  :group 'copilot-chat)

(defun copilot-chat--save-models-to-cache (models)
  "Save MODELS to disk cache."
  (when models
    (let ((cache-data `((timestamp . ,(round (float-time)))
                        (models . ,(vconcat models)))))
      (with-temp-file copilot-chat-models-cache-file
        (insert (json-serialize cache-data)))
      (when copilot-chat-debug
        (message "Saved %d models to cache %s"
                 (length models)
                 copilot-chat-models-cache-file)))))

(defun copilot-chat--load-models-from-cache ()
  "Load models from disk cache if available and not expired."
  (when (file-exists-p copilot-chat-models-cache-file)
    (with-temp-buffer
      (insert-file-contents copilot-chat-models-cache-file)
      (condition-case nil
          (let* ((cache-data (json-read-from-string (buffer-string)))
                 (timestamp (alist-get 'timestamp cache-data))
                 (current-time (round (float-time)))
                 (age (- current-time timestamp)))
            (if (< age copilot-chat-models-cache-ttl)
                (let ((models (alist-get 'models cache-data)))
                  (when copilot-chat-debug
                    (message "Loaded %d models from cache (age: %d seconds)"
                             (length models) age))
                  models)
              (when copilot-chat-debug
                (message "Cache expired (age: %d seconds, ttl: %d seconds)"
                         age copilot-chat-models-cache-ttl))
              nil))
        (error
         (when copilot-chat-debug
           (message "Error loading models from cache"))
         nil)))))

(provide 'copilot-chat-model)
;;; copilot-chat-model.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; package-lint-main-file: "copilot-chat.el"
;; End:
