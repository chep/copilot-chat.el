
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

