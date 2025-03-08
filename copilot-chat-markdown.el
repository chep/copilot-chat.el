;;; Copilot-chat --- copilot-chat-markdown.el --- copilot chat interface, markdown frontend -*- indent-tabs-mode: nil; lisp-indent-offset: 2; lexical-binding: t; package-lint-main-file: "copilot-chat.el"; -*-

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

(require 'markdown-mode)
(require 'polymode)

(require 'copilot-chat-common)
(require 'copilot-chat-prompts)

;;; Constants
(defconst copilot-chat--markdown-delimiter
  (concat "# ╭──── Chat Input ────╮")
  "The delimiter used to identify copilot chat input.")

;;; Polymode
(define-derived-mode copilot-chat-markdown-prompt-mode markdown-mode "Copilot Chat markdown Prompt"
  "Major mode for the Copilot Chat Prompt region."
  (setq major-mode 'copilot-chat-markdown-prompt-mode
    mode-name "Copilot Chat markdown prompt")
  (copilot-chat-prompt-mode))

(define-hostmode poly-copilot-markdown-hostmode
  :mode 'copilot-chat-markdown-prompt-mode)

(define-innermode poly-copilot-markdown-innermode
  :mode 'markdown-view-mode
  :head-matcher "\\`"  ; Match beginning of buffer
  :tail-matcher (concat copilot-chat--markdown-delimiter "\n")
  :head-mode 'inner
  :tail-mode 'host)

(define-polymode copilot-chat-markdown-poly-mode
  :hostmode 'poly-copilot-markdown-hostmode
  :innermodes '(poly-copilot-markdown-innermode))


;;; Functions
(defun copilot-chat--markdown-format-data (content type)
  "Format the CONTENT according to the frontend.
Argument CONTENT is the data to format.
Argument TYPE is the type of data to format: `answer` or `prompt`."
  (let ((data ""))
    (if (eq type 'prompt)
      (progn
        (setq copilot-chat--first-word-answer t)
        (setq data (concat "\n# " (format-time-string "*[%T]* You\n") (format "%s\n" content))))
      (when copilot-chat--first-word-answer
        (setq copilot-chat--first-word-answer nil)
        (setq data (concat "\n## " (concat (format-time-string "*[%T]* ") (format "Copilot(%s):\n" copilot-chat-model)))))
      (setq data (concat data content)))
    data))

(defun copilot-chat--markdown-format-code (code language)
  "Format code for markdown frontend.
Argument CODE is the code to format.
Argument LANGUAGE is the language of the code."
  (if language
    (format "\n```%s\n%s\n```\n" language code)
    code))

(defun copilot-chat--markdown-clean()
  "Clean the copilot chat markdown frontend.")

(defun copilot-chat--get-markdown-block-content-at-point ()
  "Get the content of the markdown block at point."
  (let* ((props (text-properties-at (point)))
          (face (plist-get props 'face)))
    (when (and (listp face)
            (or (memq 'markdown-pre-face face)
              (memq 'markdown-code-face face)))
      (let* ((begin-block (previous-single-property-change (point) 'face))
              (end-block (next-single-property-change (point) 'face))
              (content (when (and begin-block end-block)
                         (buffer-substring-no-properties begin-block end-block)))
              ;; Try to get language from previous text properties
              (lang-props (text-properties-at (max (- begin-block 1) (point-min))))
              (lang (plist-get lang-props 'markdown-language)))
        (when content
          (list :content content
            :language lang))))))

(defun copilot-chat--markdown-send-to-buffer()
  "Send the code block at point to buffer.
Replace selection if any."
  (let ((buffer (completing-read "Choose buffer: "
                  (mapcar #'buffer-name (buffer-list))
                  nil  ; PREDICATE
                  t    ; REQUIRE-MATCH
                  nil  ; INITIAL-INPUT
                  'buffer-name-history
                  (buffer-name (current-buffer))))
         (content (copilot-chat--get-markdown-block-content-at-point)))
    (when content
      (with-current-buffer buffer
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert (plist-get content :content))))))

(defun copilot-chat--markdown-copy ()
  "Copy the code block at point into kill ring."
  (let ((content (copilot-chat--get-markdown-block-content-at-point)))
    (when content
      (kill-new (plist-get content :content)))))

(defun copilot-chat--markdown-write (data)
  "Write DATA at the end of the chat part of the buffer."
  (if copilot-chat-follow
    (save-excursion
      (copilot-chat--markdown-goto-input)
      (forward-line -3)
      (end-of-line)
      (insert data))
    (copilot-chat--markdown-goto-input)
    (forward-line -3)
    (end-of-line)
    (insert data)))

(defun copilot-chat--markdown-goto-input ()
  "Go to the input part of the chat buffer.
The input is created if not found."
  (goto-char (point-max))
  (if (re-search-backward copilot-chat--markdown-delimiter nil t)
    (forward-line 1)
    (insert "\n\n")
    (let ((start (point))
           (inhibit-read-only t))
      (insert copilot-chat--markdown-delimiter "\n\n")
      ;; Create overlay for read-only section
      (let ((overlay (make-overlay start (1- (point)))))
        (overlay-put overlay 'read-only t)
        (overlay-put overlay 'evaporate t)))))

(defun copilot-chat--markdown-get-buffer ()
  "Create copilot-chat buffers."
  (unless (buffer-live-p copilot-chat--buffer)
    (setq copilot-chat--buffer (get-buffer-create (copilot-chat--get-buffer-name)))
    (with-current-buffer copilot-chat--buffer
      (copilot-chat-markdown-poly-mode)
      (copilot-chat--markdown-goto-input)))
  copilot-chat--buffer)


(defun copilot-chat--markdown-get-spinner-buffer ()
  "Get markdown spinner buffer."
  (let ((buffer (copilot-chat--markdown-get-buffer)))
    (if copilot-chat-follow
      buffer
      (with-current-buffer buffer
        (pm-get-buffer-of-mode 'markdown-view-mode)))))

(defun copilot-chat--markdown-insert-prompt (prompt)
  "Insert PROMPT in the chat buffer."
  (with-current-buffer (copilot-chat--markdown-get-buffer)
    (copilot-chat--markdown-goto-input)
    (unless (eobp)
      (delete-region (point) (point-max)))
    (insert prompt)))

(defun copilot-chat--markdown-pop-prompt ()
  "Get current prompt to send and clean it."
  (with-current-buffer (copilot-chat--markdown-get-buffer)
    (copilot-chat--markdown-goto-input)
    (let ((prompt (buffer-substring (point) (point-max))))
      (delete-region (point) (point-max))
      prompt)))

(defun copilot-chat--markdown-init ()
  "Initialize the copilot chat markdown frontend."
  (setq copilot-chat-prompt copilot-chat-markdown-prompt))

(provide 'copilot-chat-markdown)
;;; copilot-chat-markdown.el ends here
