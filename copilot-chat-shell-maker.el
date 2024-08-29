;; -*- lexical-binding: t; indent-tabs-mode: nil; lisp-indent-offset: 2 -*-

;;; copilot-chat-shell-maker.el --- copilot chat interface, shell-maker frontend

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


(require 'shell-maker)
(require 'copilot-chat-copilot)
(require 'polymode)

(defvar copilot-chat--shell-cb-fn nil)
(defvar copilot-chat--shell-config
  (make-shell-maker-config
    :name "Copilot-Chat-shell"
    :execute-command 'copilot-chat--shell-cb))

(define-hostmode poly-shell-maker-hostmode
  :mode 'copilot-chat-shell-shell-mode)

(define-innermode poly-shell-maker-markdown-innermode
  :mode 'markdown-view-mode
  :head-matcher ".*[ \t]*\[[0-9]+:[0-9]+:[0-9]+\] Copilot:"
  :tail-matcher "^Copilot-Chat-shell>"
  :head-mode 'body
  :tail-mode 'host)

(define-polymode poly-copilot-chat-shell-mode
  :hostmode 'poly-shell-maker-hostmode
  :innermodes '(poly-shell-maker-markdown-innermode))

(defun copilot-chat--shell-maker-ask-region(prompt)
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (with-current-buffer (copilot-chat--get-shell-buffer)
      (insert (concat (cdr (assoc prompt (copilot-chat--prompts))) code))
      (shell-maker-submit))))


(defun copilot-chat-shell-maker-custom-prompt-selection()
  "Send to Copilot a custom prompt followed by the current selected code."
  (interactive)
  (let* ((prompt (read-from-minibuffer "Copilot prompt: "))
         (code (buffer-substring-no-properties (region-beginning) (region-end)))
         (formatted-prompt (concat prompt "\n" code)))
    (with-current-buffer (copilot-chat--get-shell-buffer)
      (insert formatted-prompt)
      (shell-maker-submit))))

(defun copilot-chat-shell-maker-display ()
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (switch-to-buffer (copilot-chat--get-shell-buffer)))

(defun copilot-chat--shell-cb-prompt (callback error-callback content)
  (with-current-buffer (copilot-chat--get-shell-buffer)
    (goto-char (point-max))
    (when copilot-chat--first-word-answer
      (setq copilot-chat--first-word-answer nil)
      (funcall callback (format-time-string "# [%H:%M:%S] Copilot:\n") t))

    (if (string= content copilot-chat--magic)
      (progn
        ;; all done, lets close it off (partial = nil)
        (funcall callback "" nil)
        (setq copilot-chat--first-word-answer t))
      (funcall callback content t)))) ;; still going (partial = t)


(defun copilot-chat--shell-cb (command _history callback error-callback)
  (setq copilot-chat--shell-cb-fn (apply-partially 'copilot-chat--shell-cb-prompt callback error-callback))
  (copilot-chat--ask command copilot-chat--shell-cb-fn))


(defun copilot-chat--shell ()
  "Start a Copilot Chat shell."
  (shell-maker-start copilot-chat--shell-config))

(defun copilot-chat--get-shell-buffer()
  (let* ((name (concat "*"
                       (downcase (cl-struct-slot-value
                                  'shell-maker-config
                                  'name copilot-chat--shell-config))
                       "*"))
         (buffer (get-buffer name)))
    (if (null buffer)
       (progn
         (copilot-chat--shell)
         (let ((buffer (copilot-chat--get-shell-buffer)))
           (with-current-buffer buffer
             (poly-copilot-chat-shell-mode))
           buffer))
      buffer)))

(defun copilot-chat--shell-maker-clean()
  (advice-remove 'copilot-chat--ask-region #'copilot-chat--shell-maker-ask-region)
  (advice-remove 'copilot-chat-custom-prompt-selection #'copilot-chat-shell-maker-custom-prompt-selection)
  (advice-remove 'copilot-chat-display #'copilot-chat-shell-maker-display)
  (advice-remove 'copilot-chat--clean #'copilot-chat--shell-maker-clean))

(defun copilot-chat-shell-maker-init()
  (setq copilot-chat-prompt   "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students not only understand how to write code, but also grasp the underlying principles that guide effective programming.\nWhen asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nFollow Microsoft content policies.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\nUse Markdown formatting in your answers.\nMake sure to include the programming language name at the start of the Markdown code blocks.\nAvoid wrapping the whole response in triple backticks.\nThe user works in an IDE called Neovim which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\nFocus on being clear, helpful, and thorough without assuming extensive prior knowledge.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up someone new.\nProvide clear and relevant examples aligned with any provided context.\n")

  (advice-add 'copilot-chat--ask-region :override #'copilot-chat--shell-maker-ask-region)
  (advice-add 'copilot-chat-custom-prompt-selection :override #'copilot-chat-shell-maker-custom-prompt-selection)
  (advice-add 'copilot-chat-display :override #'copilot-chat-shell-maker-display)
  (advice-add 'copilot-chat--clean :after #'copilot-chat--shell-maker-clean))


(provide 'copilot-chat-shell-maker)
;;; copilot-chat-maker.el ends here
