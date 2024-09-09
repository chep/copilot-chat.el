;; -*- lexical-binding: t; indent-tabs-mode: nil -*-

;;; copilot-chat-copilot.el --- copilot chat engine -*- lexical-binding:t -*-

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
(require 'json)
(require 'copilot-chat-common)
(require 'copilot-chat-request)
(require 'copilot-chat-curl)

;; customs
(defgroup copilot-chat nil
  "GitHub Copilot chat."
  :group 'tools)

(defcustom copilot-chat-token-cache "~/.cache/copilot-chat/token"
  "The file where the GitHub token is cached."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-github-token-file "~/.config/copilot-chat/github-token"
  "The file where to find GitHub token."
  :type 'string
  :group 'copilot-chat)
(defcustom copilot-chat-prompt
  "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students not only understand how to write code, but also grasp the underlying principles that guide effective programming.\nWhen asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nFollow Microsoft content policies.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\nUse Markdown formatting in your answers.\nMake sure to include the programming language name at the start of the Markdown code blocks.\nAvoid wrapping the whole response in triple backticks.\nThe user works in an IDE called Neovim which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\nFocus on being clear, helpful, and thorough without assuming extensive prior knowledge.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up someone new.\nProvide clear and relevant examples aligned with any provided context.\n"
  "The prompt to use for Copilot chat."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-explain "Please write an explanation for the following code:\n"
  "The prompt used by copilot-chat-explain"
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-review "Please review the following code:\n"
  "The prompt used by copilot-chat-review"
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-doc "Please write documentation for the following code:\n"
  "The prompt used by copilot-chat-doc"
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-fix "There is a problem in this code. Please rewrite the code to show it with the bug fixed.\n"
  "The prompt used by copilot-chat-fix"
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-optimize "Please optimize the following code to improve performance and readability:\n"
  "The prompt used by copilot-chat-optimize"
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-prompt-test "Please generate tests for the following code:\n"
  "The prompt used by copilot-chat-test"
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-backend 'curl
  "Copilot chat backend. Can be 'curl or 'request."
  :type 'symbol
  :group 'copilot-chat)


;; Functions
(defun copilot-chat--prompts ()
  "Return assoc list of promts for each command"
  `((explain . ,copilot-chat-prompt-explain)
    (review . ,copilot-chat-prompt-review)
    (doc . ,copilot-chat-prompt-doc)
    (fix . ,copilot-chat-prompt-fix)
    (optimize . ,copilot-chat-prompt-optimize)
    (test . ,copilot-chat-prompt-test)))

(defun copilot-chat--get-cached-token ()
  "Get the cached GitHub token."
  (or (getenv "GITHUB_TOKEN")
      (let ((token-file (expand-file-name copilot-chat-github-token-file)))
        (when (file-exists-p token-file)
          (with-temp-buffer
            (insert-file-contents token-file)
            (buffer-substring-no-properties (point-min) (point-max)))))))


(defun copilot-chat--create ()
  "Create a new Copilot chat instance."
  (setq copilot-chat--instance(make-copilot-chat
                              :ready t
                              :github-token (copilot-chat--get-cached-token)
                              :token nil
                              :sessionid (concat (copilot-chat--uuid) (number-to-string (* (round (float-time (current-time))) 1000)))
                              :machineid (copilot-chat--machine-id)
                              :history nil
                              :buffers nil)))

(defun copilot-chat--login()
  (cond
   ((eq copilot-chat-backend 'curl)
    (copilot-chat--curl-login))
   ((eq copilot-chat-backend 'request)
    (copilot-chat--request-login))
   (t
    (error "Unknown backend: %s" copilot-chat-backend))))


(defun copilot-chat--renew-token()
(cond
   ((eq copilot-chat-backend 'curl)
    (copilot-chat--curl-renew-token))
   ((eq copilot-chat-backend 'request)
    (copilot-chat--request-renew-token))
   (t
    (error "Unknown backend: %s" copilot-chat-backend))))


(defun copilot-chat--auth()
  "Authenticate with GitHub Copilot API.
We first need github authorization (github token).
Then we need a session token."
  (unless (copilot-chat-github-token copilot-chat--instance)
    (copilot-chat--login))

  (when (null (copilot-chat-token copilot-chat--instance))
    ;; try to load token from ~/.cache/copilot-chat-token
    (let ((token-file (expand-file-name copilot-chat-token-cache)))
      (when (file-exists-p token-file)
        (with-temp-buffer
          (insert-file-contents token-file)
          (setf (copilot-chat-token copilot-chat--instance) (json-read-from-string (buffer-substring-no-properties (point-min) (point-max))))))))

  (when (or (null (copilot-chat-token copilot-chat--instance))
            (> (round (float-time (current-time))) (alist-get 'expires_at (copilot-chat-token copilot-chat--instance))))
    (copilot-chat--renew-token)))

(defun copilot-chat--ask (prompt callback)
  "Ask a question to Copilot."
  (let* ((history (copilot-chat-history copilot-chat--instance))
         (new-history (cons (list prompt "user") history)))
    (copilot-chat--auth)
    (cond
     ((eq copilot-chat-backend 'curl)
      (copilot-chat--curl-ask prompt callback))
     ((eq copilot-chat-backend 'request)
      (copilot-chat--request-ask prompt callback))
     (t
      (error "Unknown backend: %s" copilot-chat-backend)))
    (setf (copilot-chat-history copilot-chat--instance) new-history)))

(defun copilot-chat--add-buffer (buffer)
  (unless (memq buffer (copilot-chat-buffers copilot-chat--instance))
    (let* ((buffers (copilot-chat-buffers copilot-chat--instance))
           (new-buffers (cons buffer buffers)))
      (setf (copilot-chat-buffers copilot-chat--instance) new-buffers))))

(defun copilot-chat--clear-buffers ()
  (setf (copilot-chat-buffers copilot-chat--instance) nil))

(defun copilot-chat--del-buffer (buffer)
  (when (memq buffer (copilot-chat-buffers copilot-chat--instance))
    (setf (copilot-chat-buffers copilot-chat--instance)
          (delete buffer (copilot-chat-buffers copilot-chat--instance)))))

(defun copilot-chat--get-buffers ()
  (copilot-chat-buffers copilot-chat--instance))

(defun copilot-chat--ready-p()
  (copilot-chat-ready copilot-chat--instance))

(provide 'copilot-chat-copilot)
;;; copilot-chat-copilot.el ends here
