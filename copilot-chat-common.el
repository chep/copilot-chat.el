;;; copilot-chat --- copilot-chat-common.el --- copilot chat variables and const -*- indent-tabs-mode: nil; lexical-binding:t -*-

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

;; constants
(defconst copilot-chat--magic "#cc#done#!$")
(defconst copilot-chat--buffer "*Copilot-chat*")
(defconst copilot-chat--prompt-buffer "*Copilot-chat-prompt*")


;; customs
(defgroup copilot-chat nil
  "GitHub Copilot chat."
  :group 'tools)

(defcustom copilot-chat-prompt
  "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students not only understand how to write code, but also grasp the underlying principles that guide effective programming.\nWhen asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nFollow Microsoft content policies.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\nUse Markdown formatting in your answers.\nMake sure to include the programming language name at the start of the Markdown code blocks.\nAvoid wrapping the whole response in triple backticks.\nThe user works in an IDE called Neovim which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\nFocus on being clear, helpful, and thorough without assuming extensive prior knowledge.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up someone new.\nProvide clear and relevant examples aligned with any provided context.\n"
  "The prompt to use for Copilot chat."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-github-token-file "~/.config/copilot-chat/github-token"
  "The file where to find GitHub token."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-token-cache "~/.cache/copilot-chat/token"
  "The file where the GitHub token is cached."
  :type 'string
  :group 'copilot-chat)

;;  OpenAI models: https://platform.openai.com/docs/models
(defcustom copilot-chat-model "gpt-4o"
  "The model to use for Copilot chat."
  :type '(choice (const :tag "GPT-4o" "gpt-4o")
                 (const :tag "Claude 3.5 Sonnet" "claude-3.5-sonnet")
                 (const :tag "GPT-4o1-(preview)" "o1-preview"))
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-suffix nil
  "Suffix to be added to the end of the prompt before sending to Copilot Chat. For Example: Reply in Chinese (or any other language)
If nil, no suffix will be added."
  :type 'string
  :group 'copilot-chat)

;; structs
(cl-defstruct copilot-chat
  ready
  github-token
  token
  sessionid
  machineid
  history
  buffers)

;; variables
(defvar copilot-chat--instance
  (make-copilot-chat
   :ready nil
   :github-token nil
   :token nil
   :sessionid nil
   :machineid nil
   :history nil
   :buffers nil))

(defvar copilot-chat--first-word-answer t)

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

(defun copilot-chat--create-req(prompt no-context)
  "Create a request for Copilot.
Argument PROMPT Copilot prompt to send.
Argument NOCONTEXT tells copilot-chat to not send history and buffers."
  (let ((messages nil))
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

    (json-encode `(("messages" . ,(vconcat messages))
                   ("top_p" . 1)
                   ("model" . ,copilot-chat-model)
                   ("stream" . t)
                   ("n" . 1)
                   ("intent" . t)
                   ("temperature" . 0.1)))))


(provide 'copilot-chat-common)
;;; copilot-chat-common.el ends here
