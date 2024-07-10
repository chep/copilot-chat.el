(require 'copilot-chat-copilot)

(defvar copilot-chat-buffer (get-buffer-create "*Copilot-chat*"))
(defvar copilot-chat-prompt-buffer (get-buffer-create "*Copilot-chat-prompt*"))


(defun copilot-chat-mode ()
  "Major mode for Copilot Chat buffer."
  (interactive)
  (kill-all-local-variables)
  (use-local-map copilot-chat-mode-map)
  (setq major-mode 'copilot-chat-mode)
  (setq mode-name "Copilot Chat")
  (setq buffer-read-only t)
  (run-hooks 'copilot-chat-mode-hook))

(defvar copilot-chat-mode-map (make-keymap)
  "Keymap for Copilot Chat major mode.")
(define-key copilot-chat-mode-map (kbd "C-c q") 'bury-buffer)

(define-derived-mode copilot-chat-mode special-mode "Copilot Chat"
  "Major mode for the Copilot Chat buffer."
  (read-only-mode 1))

(defun copilot-chat-mode-prompt ()
  "Major mode for Copilot Chat Prompt buffer."
  (interactive)
  (kill-all-local-variables)
  (use-local-map copilot-chat-mode-prompt-map)
  (setq major-mode 'copilot-chat-mode-prompt)
  (setq mode-name "Copilot Chat Prompt")
  (run-hooks 'copilot-chat-mode-prompt-hook))

(defvar copilot-chat-mode-prompt-map (make-keymap)
  "Keymap for Copilot Chat Prompt major mode.")
(define-key copilot-chat-mode-prompt-map (kbd "C-c RET") 'copilot-chat-prompt-send)
(define-key copilot-chat-mode-prompt-map (kbd "C-c q") '(lambda() (bury-buffer) (delete-window)))

(define-derived-mode copilot-chat-mode-prompt text-mode "Copilot Chat Prompt"
  "Major mode for the Copilot Chat Prompt buffer.")


(defun copilot-chat-write-buffer(content type)
  "Write content to the Copilot Chat buffer."
  (with-current-buffer copilot-chat-buffer
	(let ((inhibit-read-only t))
	(goto-char (point-max))
	(insert (concat (propertize (format-time-string "%H:%M:%S") 'face 'font-lock-comment-face) " "))
	(insert (propertize (format "%s\n" content) 'face (if (eq type 'prompt) 'font-lock-keyword-face 'font-lock-string-face))))))


(defun copilot-chat-prompt-cb (content)
  (copilot-chat-write-buffer content 'answer))

(defun copilot-chat-prompt-send ()
  "Function to send the prompt content."
  (interactive)
  (with-current-buffer copilot-chat-prompt-buffer
	(let ((prompt (buffer-substring-no-properties (point-min) (point-max))))
	  (erase-buffer)
	  (copilot-chat-write-buffer prompt 'prompt)
	  (copilot-chat-ask prompt 'copilot-chat-prompt-cb))))


(defun copilot-chat-ask-region(prompt)
  (let ((code (buffer-substring-no-properties (region-beginning) (region-end))))
	(with-current-buffer copilot-chat-prompt-buffer
	  (erase-buffer)
	  (insert (concat (cdr (assoc prompt copilot-chat-prompts)) code)))
	(copilot-chat-prompt-send)))

(defun copilot-chat-explain()
  "Ask Copilot to explain the current selected code."
  (interactive)
  (copilot-chat-ask-region 'explain))

(defun copilot-chat-review()
  "Ask Copilot to review the current selected code."
  (interactive)
  (copilot-chat-ask-region 'review))

(defun copilot-chat-doc()
  "Ask Copilot to write documentation for the current selected code."
  (interactive)
  (copilot-chat-ask-region 'doc))

(defun copilot-chat-fix()
  "Ask Copilot to fix the current selected code."
  (interactive)
  (copilot-chat-ask-region 'fix))

(defun copilot-chat-optimize()
  "Ask Copilot to optimize the current selected code."
  (interactive)
  (copilot-chat-ask-region 'optimize))

(defun copilot-chat-test()
  "Ask Copilot to generate tests for the current selected code."
  (interactive)
  (copilot-chat-ask-region 'test))

(defun copilot-chat-custom-prompt-selection()
  "Send to Copilot a custom prompt followed by the current selected code."
  (interactive)
  (let* ((prompt (read-from-minibuffer "Copilot prompt: "))
		(code (buffer-substring-no-properties (region-beginning) (region-end)))
		(formatted-prompt (concat prompt "\n" code)))
	(with-current-buffer copilot-chat-prompt-buffer
	  (erase-buffer)
	  (insert formatted-prompt))
	(copilot-chat-prompt-send)))



;;;###autoload
(defun copilot-chat ()
  "Open Copilot Chat buffer."
  (interactive)
  (copilot-chat-create)
  (let ((buffer copilot-chat-buffer))
    (with-current-buffer buffer
      (copilot-chat-mode))
    (switch-to-buffer buffer)))

;;;###autoload
(defun copilot-chat-prompt ()
  "Open Copilot Chat Prompt buffer."
  (interactive)
  (let ((buffer copilot-chat-prompt-buffer))
    (with-current-buffer buffer
      (copilot-chat-mode-prompt))
    (switch-to-buffer buffer)))

(defun copilot-chat-display ()
  (interactive)
  (let ((chat-buffer (get-buffer-create copilot-chat-buffer))
        (prompt-buffer (get-buffer-create copilot-chat-prompt-buffer)))
    (with-current-buffer chat-buffer
      (copilot-chat-mode))
    (with-current-buffer prompt-buffer
      (copilot-chat-mode-prompt))
    (switch-to-buffer chat-buffer)
    (let ((split-window-preferred-function nil)
          (split-height-threshold nil)
          (split-width-threshold nil))
      (split-window-below (floor (* 0.8 (window-total-width)))))
    (other-window 1)
    (switch-to-buffer prompt-buffer)
    (other-window -1)))

(provide 'copilot-chat)

;; To ensure autoloads are set correctly, add the following to your Emacs configuration file:
;; (autoload 'copilot-chat "copilot-chat" nil t)
;; (autoload 'copilot-chat-prompt "copilot-chat" nil t)
