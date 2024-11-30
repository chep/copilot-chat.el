
;; tests for copilot-chat--build-prompt-suffix

(require 'ert)

(defvar copilot-chat-prompt-suffix nil)

(defun test-major-mode (mode)
  "Helper function to set the major mode for testing."
  (with-temp-buffer
    (funcall mode)
    (copilot-chat--build-prompt-suffix)))

(ert-deftest test-copilot-chat--build-prompt-suffix-no-suffix ()
  "Test copilot-chat--build-prompt-suffix with no custom suffix."
  (let ((copilot-chat-prompt-suffix nil))
    (should (equal (test-major-mode 'emacs-lisp-mode)
                   "current programming language is: emacs-lisp"))))

(ert-deftest test-copilot-chat--build-prompt-suffix-with-suffix ()
  "Test copilot-chat--build-prompt-suffix with a custom suffix."
  (let ((copilot-chat-prompt-suffix "additional info"))
    (should (equal (test-major-mode 'emacs-lisp-mode)
                   "current programming language is: emacs-lisp, additional info"))))

(ert-deftest test-copilot-chat--build-prompt-suffix-different-mode ()
  "Test copilot-chat--build-prompt-suffix with different major modes."
  (let ((copilot-chat-prompt-suffix "extra details"))
    (should (equal (test-major-mode 'python-mode)
                   "current programming language is: python, extra details"))
    (should (equal (test-major-mode 'c-mode)
                   "current programming language is: c, extra details"))))

;; tests for copilot-chat--create-req

(require 'ert)
(require 'json)

(defvar copilot-chat--instance nil)
(defvar copilot-chat-prompt "system prompt")
(defvar copilot-chat-model "gpt-4")

(defun make-copilot-chat (&rest args)
  (apply 'list args))

(defun copilot-chat-history (instance)
  (plist-get instance :history))

(defun copilot-chat-buffers (instance)
  (plist-get instance :buffers))

(defun set-copilot-chat-history (instance history)
  (plist-put instance :history history))

(defun set-copilot-chat-buffers (instance buffers)
  (plist-put instance :buffers buffers))

(defun setup-copilot-chat-instance ()
  (setq copilot-chat--instance (make-copilot-chat :history nil :buffers nil)))

(ert-deftest test-copilot-chat--create-req-no-context ()
  "Test copilot-chat--create-req with no-context."
  (setup-copilot-chat-instance)
  (let ((result (copilot-chat--create-req "Test prompt" t)))
    (should (equal (json-read-from-string result)
                   '(("messages" . [((content . "Test prompt") (role . "user"))
                                    ((content . "system prompt") (role . "system"))])
                     ("top_p" . 1)
                     ("model" . "gpt-4")
                     ("stream" . t)
                     ("n" . 1)
                     ("intent" . t)
                     ("temperature" . 0.1))))))

(ert-deftest test-copilot-chat--create-req-with-context ()
  "Test copilot-chat--create-req with context."
  (setup-copilot-chat-instance)
  (set-copilot-chat-history copilot-chat--instance '(("history content" "user")))
  (let ((buffer (get-buffer-create "*test-buffer*")))
    (with-current-buffer buffer
      (insert "buffer content"))
    (set-copilot-chat-buffers copilot-chat--instance (list buffer))
    (let ((result (copilot-chat--create-req "Test prompt" nil)))
      (should (equal (json-read-from-string result)
                     '(("messages" . [((content . "Test prompt") (role . "user"))
                                      ((content . "history content") (role . "user"))
                                      ((content . "buffer content") (role . "user"))
                                      ((content . "system prompt") (role . "system"))])
                       ("top_p" . 1)
                       ("model" . "gpt-4")
                       ("stream" . t)
                       ("n" . 1)
                       ("intent" . t)
                       ("temperature" . 0.1)))))
    (kill-buffer buffer)))
