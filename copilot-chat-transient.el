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
(transient-define-prefix
 copilot-chat-transient () "Copilot chat command menu."
 [["Commands"
   ("d" "Display chat" copilot-chat-display)
   ("h" "Hide chat" copilot-chat-hide)
   ("x" "Reset" copilot-chat-reset)
   ("g" "Go to buffer" copilot-chat-switch-to-buffer)
   ("q" "Quit" transient-quit-one)]
  ["Instance"
   ("M" "Set model" copilot-chat-set-model)
   ("C" "Set commit model" copilot-chat-set-commit-model)
   ("S" "Save chat" copilot-chat-save)
   ("L" "Load chat" copilot-chat-load)
   ("k" "Kill instance" copilot-chat-kill-instance)]
  ["Actions"
   ("p" "Custom prompt" copilot-chat-custom-prompt-selection)
   ("i" "Ask and insert" copilot-chat-ask-and-insert)
   ("m" "Insert commit message" copilot-chat-insert-commit-message)]
  ["Data"
   ("y" "Yank last code block" copilot-chat-yank)
   ("s" "Send code to buffer" copilot-chat-send-to-buffer)]
  ["Tools"
   ("b" "Buffers" copilot-chat-transient-buffers)
   ("c" "Code helpers" copilot-chat-transient-code)]])

;;;###autoload (autoload 'copilot-chat-transient-buffers "copilot-chat" nil t)
(transient-define-prefix
 copilot-chat-transient-buffers () "Copilot chat buffers menu."
 [["Buffers"
   ("a" "Add buffers" copilot-chat-add-buffers)
   ("A"
    "Add all buffers in current frame"
    copilot-chat-add-buffers-in-current-window)
   ("d" "Delete buffers" copilot-chat-del-buffers)
   ("D" "Delete all buffers" copilot-chat-list-clear-buffers)
   ("f" "Add files under current directory" copilot-chat-add-files-under-dir)
   ("l" "Display buffer list" copilot-chat-list)
   ("c" "Clear buffers" copilot-chat-list-clear-buffers)
   ("q" "Quit" transient-quit-one)]])

;;;###autoload (autoload 'copilot-chat-transient-code "copilot-chat" nil t)
(transient-define-prefix
 copilot-chat-transient-code () "Copilot chat code helpers menu."
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


(defun copilot-chat--index-to-key (index)
  "Convert INDEX to a string key for transient suffixes."
  (cond
   ((< index 10)
    (format "%d" index))
   ((< index 36)
    (char-to-string (+ ?a (- index 10))))
   ((< index 62)
    (char-to-string (+ ?A (- index 10))))
   (t
    (error "Index %d is out of range for transient suffixes" index))))

(defun copilot-chat--mcp-generate-server-suffixes ()
  "Generate dynamic switches for servers."
  (let ((suffixes '())
        (index 0)
        (instance (copilot-chat--current-instance)))
    ;; Ajouter chaque serveur comme switch
    (dolist (server (mapcar 'car mcp-hub-servers))
      (push (list
             (copilot-chat--index-to-key index)
             (format "Add %s" server)
             (format "%s" server)
             :init-value
             (lambda (obj)
               (when (member
                      (slot-value obj 'argument)
                      (copilot-chat-mcp-servers instance))
                 (setf (slot-value obj 'value) (slot-value obj 'argument)))))
            suffixes)
      (setq index (1+ index)))

    ;; Ajouter l'option "ALL"
    (push (list (copilot-chat--index-to-key index) "Add All" "ALL") suffixes)
    (push (list (copilot-chat--index-to-key (1+ index)) "Clear All" "CLEAR")
          suffixes)

    ;; Inverser pour avoir l'ordre correct
    (nreverse suffixes)))

(defun copilot-chat--mcp-handle-selection (servers)
  "Handle selected SERVERS from arguments."
  (interactive (list (transient-args 'copilot-chat-mcp-servers-transient)))
  (let ((instance (copilot-chat--current-instance)))
    (cond
     ((member "ALL" servers)
      (setf (copilot-chat-mcp-servers instance) (mapcar 'car mcp-hub-servers)))
     ((member "CLEAR" servers)
      (setf (copilot-chat-mcp-servers instance) nil))
     (t
      (setf (copilot-chat-mcp-servers instance) servers)))
    (copilot-chat--activate-mcp-servers instance)))

;;;###autoload (autoload 'copilot-chat-mcp-servers-transient "copilot-chat" nil t)
(transient-define-prefix
 copilot-chat-mcp-servers-transient () "Copilot chat MCP servers menu."
 ["MCP servers:"
  :class transient-column
  :setup-children
  (lambda (_)
    (transient-parse-suffixes
     transient--prefix (copilot-chat--mcp-generate-server-suffixes)))]
 [["Actions"
   ("RET" "Validate" copilot-chat--mcp-handle-selection)
   ("q" "Cancel" transient-quit-one)]])

;;;###autoload (autoload 'copilot-chat-set-mcp-servers "copilot-chat" nil t)
(defalias 'copilot-chat-set-mcp-servers 'copilot-chat-mcp-servers-transient)

(provide 'copilot-chat-transient)
;;; copilot-chat-transient.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; checkdoc-verb-check-experimental-flag: nil
;; End:
