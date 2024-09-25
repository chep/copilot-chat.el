;;; copilot-chat --- copilot-chat-shell-maker.el --- copilot chat interface, shell-maker frontend -*- indent-tabs-mode: nil; lisp-indent-offset: 2; lexical-binding: t -*-

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

(defvar copilot-chat--shell-cb-fn nil)
(defvar copilot-chat--shell-config
  (make-shell-maker-config
    :name "Copilot-Chat"
    :execute-command 'copilot-chat--shell-cb))
(defvar copilot-chat--shell-maker-answer-point 0
  "Start of the current answer.")

(defconst copilot-chat--shell-maker-temp-buffer "*copilot-chat-shell-maker-temp*")

(defun copilot-chat--shell-maker-ask-region(prompt)
  "Send to Copilot a prompt followed by the current selected code.
Argument PROMPT is the prompt to send to Copilot."
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
    (with-current-buffer copilot-chat--buffer
      (insert (concat (cdr (assoc prompt (copilot-chat--prompts))) code))
      (shell-maker-submit))))

(defun copilot-chat-shell-maker-custom-prompt-selection()
  "Send to Copilot a custom prompt followed by the current selected code."
  (interactive)
  (let* ((prompt (read-from-minibuffer "Copilot prompt: "))
         (code (buffer-substring-no-properties (region-beginning) (region-end)))
         (formatted-prompt (concat prompt "\n" code)))
    (with-current-buffer copilot-chat--buffer
      (insert formatted-prompt)
      (shell-maker-submit))))

(defun copilot-chat-shell-maker-display ()
  "Display copilot chat buffer."
  (interactive)
  (unless (copilot-chat--ready-p)
    (copilot-chat-reset))
  (let ((buffer (get-buffer copilot-chat--buffer))
        (tempb (get-buffer-create copilot-chat--shell-maker-temp-buffer))
        (inhibit-read-only t))
    (unless buffer
      (setq buffer (copilot-chat--shell)))
    (with-current-buffer tempb
      (markdown-view-mode))
    (pop-to-buffer buffer)))

(defun copilot-chat--shell-maker-font-lock-faces ()
  "Replace faces by font-lock-faces."
  (with-current-buffer copilot-chat--shell-maker-temp-buffer
    (let ((inhibit-read-only t))
      (font-lock-ensure)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((next-change (or (next-property-change (point) nil (point-max)) (point-max)))
               (face (get-text-property (point) 'face)))
          (when face
            (font-lock-append-text-property (point) next-change 'font-lock-face face))
          (goto-char next-change))))))

(defun copilot-chat--shell-maker-copy-faces()
  "Apply faces to the copilot chat buffer."
  (with-current-buffer copilot-chat--shell-maker-temp-buffer
    (save-restriction
	  (widen)
      (font-lock-ensure)
      (copilot-chat--shell-maker-font-lock-faces)
      (let ((content (buffer-substring (point-min) (point-max))))
        (with-current-buffer copilot-chat--buffer
          (goto-char (1+ copilot-chat--shell-maker-answer-point))
          (insert content)
          (delete-region (point) (+ (point) (length content)))
          (goto-char (point-max)))))))

(defun copilot-chat--shell-cb-prompt (callback _error-callback content)
  "Callback for Copilot Chat shell-maker.
Argument CALLBACK is the callback function to call.
Argument ERROR-CALLBACK is the error callback function to call.
Argument CONTENT is copilot chat answer."
  (with-current-buffer copilot-chat--buffer
    (goto-char (point-max))
    (when copilot-chat--first-word-answer
      (setq copilot-chat--first-word-answer nil)
      (let ((str (format-time-string "# [%H:%M:%S] Copilot:\n"))
             (inhibit-read-only t))
        (with-current-buffer copilot-chat--shell-maker-temp-buffer
          (insert str))
        (funcall callback str t)))
    (if (string= content copilot-chat--magic)
      (progn
        (funcall callback "" nil) ; the end, partial = nil
        (copilot-chat--shell-maker-copy-faces)
        (setq copilot-chat--first-word-answer t))
      (progn
        (with-current-buffer copilot-chat--shell-maker-temp-buffer
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert content)))
        (funcall callback content t))))) ; partial = t


(defun copilot-chat--shell-cb (command _history callback error-callback)
  "Callback for Copilot Chat shell-maker.
Argument COMMAND is the command to send to Copilot.
Argument CALLBACK is the callback function to call.
Argument ERROR-CALLBACK is the error callback function to call."
  (setq
    copilot-chat--shell-cb-fn
    (apply-partially 'copilot-chat--shell-cb-prompt callback error-callback)
    copilot-chat--shell-maker-answer-point (point))
  (let ((inhibit-read-only t))
    (with-current-buffer copilot-chat--shell-maker-temp-buffer
      (erase-buffer)))
  (copilot-chat--ask command copilot-chat--shell-cb-fn))

(defun copilot-chat--shell ()
  "Start a Copilot Chat shell."
  (shell-maker-start
    copilot-chat--shell-config
    t nil t
    copilot-chat--buffer))

(defun copilot-chat--shell-maker-clean()
  "Clean the copilot chat shell-maker frontend."
  (advice-remove 'copilot-chat--ask-region #'copilot-chat--shell-maker-ask-region)
  (advice-remove 'copilot-chat-custom-prompt-selection #'copilot-chat-shell-maker-custom-prompt-selection)
  (advice-remove 'copilot-chat-display #'copilot-chat-shell-maker-display)
  (advice-remove 'copilot-chat--clean #'copilot-chat--shell-maker-clean))

(defun copilot-chat-shell-maker-init()
  "Initialize the copilot chat shell-maker frontend."
  (setq copilot-chat-prompt   "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students not only understand how to write code, but also grasp the underlying principles that guide effective programming.\nWhen asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nFollow Microsoft content policies.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\nUse Markdown formatting in your answers.\nMake sure to include the programming language name at the start of the Markdown code blocks.\nAvoid wrapping the whole response in triple backticks.\nThe user works in an IDE called Neovim which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\nFocus on being clear, helpful, and thorough without assuming extensive prior knowledge.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up someone new.\nProvide clear and relevant examples aligned with any provided context.\n")

  (advice-add 'copilot-chat--ask-region :override #'copilot-chat--shell-maker-ask-region)
  (advice-add 'copilot-chat-custom-prompt-selection :override #'copilot-chat-shell-maker-custom-prompt-selection)
  (advice-add 'copilot-chat-display :override #'copilot-chat-shell-maker-display)
  (advice-add 'copilot-chat--clean :after #'copilot-chat--shell-maker-clean))


(provide 'copilot-chat-shell-maker)

;;; copilot-chat-shell-maker.el ends here
