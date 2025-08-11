;;; copilot-chat --- copilot-chat-mcp.el --- mcp servers management -*- lexical-binding: t; -*-

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
(require 'mcp-hub)
(require 'copilot-chat-instance)

(declare-function copilot-chat--ask "copilot-chat-copilot")
(declare-function copilot-chat-prompt-cb "copilot-chat-prompt-mode")


(cl-defstruct
 copilot-chat-function
 "Structure to hold function information."
 (name "" :type string)
 (arguments "" :type string)
 (id "" :type string))

(defun copilot-chat--append-vector-to-function (vector function)
  "Append elements from VECTOR to FUNCTION."
  (dolist (item (elt vector 0))
    (cond
     ;; if `function` is présent
     ((eq (car item) 'function)
      (let ((name (alist-get 'name item))
            (args (alist-get 'arguments item)))
        (when name
          (setf (copilot-chat-function-name function) name))
        (when args
          (setf (copilot-chat-function-arguments function)
                (concat (copilot-chat-function-arguments function) args)))))
     ;; If `id` is present
     ((eq (car item) 'id)
      (setf (copilot-chat-function-id function) (cdr item))))))

(defun copilot-chat--extract-json-objects (json-string)
  "Extract individual JSON objects from JSON-STRING."
  (let ((objects '())
        (start 0)
        (brace-count 0)
        (in-object nil)
        (in-string nil)
        (escape-next nil)
        (length (length json-string)))

    (dotimes (i length)
      (let ((char (aref json-string i)))
        (cond
         ;; Handle escape sequences
         (escape-next
          (setq escape-next nil))

         ;; Check for escape character
         ((= char ?\\)
          (setq escape-next t))

         ;; Toggle string state (only if not escaped)
         ((= char ?\")
          (setq in-string (not in-string)))

         ;; Only count braces outside of strings
         ((and (not in-string) (= char ?{))
          (when (= brace-count 0)
            (setq in-object t)
            (setq start i))
          (setq brace-count (1+ brace-count)))

         ;; Closing brace (only if not in a string)
         ((and (not in-string) (= char ?}) (> brace-count 0))
          (setq brace-count (1- brace-count))
          (when (and in-object (= brace-count 0))
            (push (substring json-string start (1+ i)) objects)
            (setq in-object nil))))))

    ;; Return objects in original order
    (nreverse objects)))

(defun copilot-chat--mcp-find-connection (instance function)
  "Find the MCP connection for the given FUNCTION in INSTANCE."
  (catch 'break
    (dolist (server (copilot-chat-mcp-servers instance))
      (let ((connection (gethash server mcp-server-connections)))
        (when connection
          (let ((tools (mcp--tools connection)))
            (when (seq-some
                   (lambda (item)
                     (string=
                      (plist-get item :name)
                      (copilot-chat-function-name function)))
                   tools)
              (throw 'break connection))))))
    nil))

(defun copilot-chat--call-function (instance function callback)
  "Call the FUNCTION and manage the result.
INSTANCE is the copilot chat instance."
  (let* ((connection (copilot-chat--mcp-find-connection instance function))
         (name (copilot-chat-function-name function))
         (arguments (copilot-chat-function-arguments function))
         (arglist (copilot-chat--extract-json-objects arguments)))
    (unless arglist
      (setq arglist (list nil)))
    (dolist (arg arglist)
      (mcp-async-call-tool
       connection name
       (when arg
         (json-parse-string arg :object-type 'alist :false-object :json-false))
       (lambda (result)
         (copilot-chat--ask
          instance
          (append
           `(:role
             "tool"
             :tool_call_id ,(copilot-chat-function-id function))
           result)
          callback))
       (lambda (_ msg)
         (message "Error calling function %s: %s" name msg)
         (copilot-chat--ask
          instance
          `(:role
            "tool"
            :tool_call_id ,(copilot-chat-function-id function)
            :content ,msg)
          callback))))))

(defun copilot-chat--activate-mcp-servers (instance)
  "Start the MCP server connections for INSTANCE."
  (let ((servers (copilot-chat-mcp-servers instance)))
    (dolist (server-name servers)
      (dolist (server mcp-hub-servers)
        (when (string= server-name (car server))
          (unless (mcp--server-running-p server)
            (apply #'mcp-connect-server
                   (append
                    (list (car server)) (cdr server)
                    (list
                     :initial-callback (lambda (_))
                     :tools-callback
                     (lambda (server _)
                       (message
                        (format "MCP server %s started"
                                (slot-value server 'name))))
                     :prompts-callback nil
                     :resources-callback nil
                     :resources-templates-callback nil
                     :error-callback
                     (lambda (_ err)
                       (error (concat "MCP server start error :" err))))))))))))

(defun copilot-chat--get-tools (instance)
  "Return the list of tools from the MCP servers managed in INSTANCE."
  (let ((all-tools nil))
    (dolist (server (copilot-chat-mcp-servers instance))
      (let ((connection (gethash server mcp-server-connections)))
        (when connection
          (let ((tools (mcp--tools connection)))
            (mapc
             (lambda (tool)
               (push `(:type
                       "function"
                       :function
                       (:name
                        ,(plist-get tool :name)
                        :description ,(plist-get tool :description)
                        :parameters
                        (:type
                         ,(plist-get (plist-get tool :inputSchema) :type)
                         :properties
                         ,(plist-get
                           (plist-get tool :inputSchema)
                           :properties))))
                     all-tools))
             tools)))))
    all-tools))

(provide 'copilot-chat-mcp)
;;; copilot-chat-mcp.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
