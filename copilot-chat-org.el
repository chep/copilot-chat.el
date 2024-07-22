;; -*- lexical-binding: t -*-
;; -*- indent-tabs-mode: nil -*-
;;; copilot-chat-org.el --- copilot chat interface, org frontend

;; Copyright (C) 2024  Cédric Chépied

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

(require 'org)


(defun copilot-chat-org-write(content type)
  "Write content to the Copilot Chat buffer."
  (with-current-buffer copilot-chat-buffer
	(let ((inhibit-read-only t))
	  (goto-char (point-max))
	  (if (eq type 'prompt)
		(progn
		  (insert (concat "* " (format-time-string "*[%H:%M:%S]* ") (format "%s\n" content)))
		  (setq copilot-chat-first-word-answer t))
		(when copilot-chat-first-word-answer
		  (insert (concat "** " (format-time-string "*[%H:%M:%S]* ")))
		  (setq copilot-chat-first-word-answer nil))
		(insert content)))))

(defun copilot-chat-org-clean()
  (advice-remove 'copilot-chat-write-buffer #'copilot-chat-org-write)
  (advice-remove 'copilot-chat-clean #'copilot-chat-org-clean))


(defun copilot-chat-org-init()
  (setq copilot-chat-prompt "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students not only understand how to write code, but also grasp the underlying principles that guide effective programming.\nWhen asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nFollow Microsoft content policies.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\nUse Emacs org mode formatting in your answers, **not Markdown**.\n**NEVER USE MARKDOWN SYNTAX!**\nStart your answer with two asteriks to create a second level headline and make sure to include the programming language name at the start of the org mode code blocks.\nAvoid wrapping the whole response in the block code.\nThe user works in an IDE called Emacs which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsure about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code readability, performance, etc.\n\nFocus on being clear, helpful, and thorough without assuming extensive prior knowledge.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up someone new.\nProvide clear and relevant examples aligned with any provided context.\n**I REPEAT: DO NOT USE MARDOWN SYNTAX, ONLY EMACS ORG!\n")

  (define-derived-mode copilot-chat-mode org-mode "Copilot Chat"
	"Major mode for the Copilot Chat buffer."
	(read-only-mode 1))

  (define-derived-mode copilot-chat-prompt-mode org-mode "Copilot Chat Prompt")

  (advice-add 'copilot-chat-write-buffer :override #'copilot-chat-org-write)
  (advice-add 'copilot-chat-clean :after #'copilot-chat-org-clean))

(provide 'copilot-chat-org)
;;; copilot-chat-org.el ends here
