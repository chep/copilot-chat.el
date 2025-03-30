;;; copilot-chat --- copilot-chat-transient.el  --- copilot chat transient functions -*- lexical-binding: t; -*-

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

(require 'transient)

(require 'copilot-chat-command)

;;;###autoload (autoload 'copilot-chat-transient "copilot-chat" nil t)
(transient-define-prefix copilot-chat-transient ()
  "Copilot chat command menu."
  [["Commands"
    ("d" "Display chat" copilot-chat-display)
    ("h" "Hide chat" copilot-chat-hide)
    ("r" "Reset & reopen" (lambda ()
                            (interactive)
                            (copilot-chat-reset)
                            (copilot-chat-display)))
    ("x" "Reset" copilot-chat-reset)
    ("g" "Go to buffer" copilot-chat-switch-to-buffer)
    ("M" "Set model" copilot-chat-set-model)
    ("q" "Quit" transient-quit-one)]
   ["Actions"
    ("p" "Custom prompt" copilot-chat-custom-prompt-selection)
    ("i" "Ask and insert" copilot-chat-ask-and-insert)
    ("m" "Insert commit message" copilot-chat-insert-commit-message)]
   ["Data"
    ("y" "Yank last code block" copilot-chat-yank)
    ("s" "Send code to buffer" copilot-chat-send-to-buffer)]
   ["Tools"
    ("b" "Buffers" copilot-chat-transient-buffers)
    ("c" "Code helpers" copilot-chat-transient-code)]
   ])

;;;###autoload (autoload 'copilot-chat-transient-buffers "copilot-chat" nil t)
(transient-define-prefix copilot-chat-transient-buffers ()
  "Copilot chat buffers menu."
  [["Buffers"
    ("a" "Add buffers" copilot-chat-add-buffers)
    ("A" "Add all buffers in current frame" copilot-chat-add-buffers-in-current-window)
    ("d" "Delete buffers" copilot-chat-del-buffers)
    ("D" "Delete all buffers" copilot-chat-list-clear-buffers)
    ("f" "Add files under current directory" copilot-chat-add-files-under-dir)
    ("l" "Display buffer list" copilot-chat-list)
    ("c" "Clear buffers" copilot-chat-list-clear-buffers)
    ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'copilot-chat-transient-code "copilot-chat" nil t)
(transient-define-prefix copilot-chat-transient-code ()
  "Copilot chat code helpers menu."
  [["Code helpers"
    ("e" "Explain" copilot-chat-explain)
    ("E" "Explain symbol" copilot-chat-explain-symbol-at-line)
    ("r" "Review" copilot-chat-review)
    ("d" "Doc" copilot-chat-doc)
    ("f" "Fix" copilot-chat-fix)
    ("o" "Optimize" copilot-chat-optimize)
    ("t" "Test" copilot-chat-test)
    ("F" "Explain function" copilot-chat-explain-defun)
    ("c" "Custom prompt function" copilot-chat-custom-prompt-function)
    ("R" "Review whole buffer" copilot-chat-review-whole-buffer)
    ("q" "Quit" transient-quit-one)]])

(provide 'copilot-chat-transient)
;;; copilot-chat-transient.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; package-lint-main-file: "copilot-chat.el"
;; End:
