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


(defun copilot-chat--update-function (function args)
  "Update FUNCTION by appending ARGS to its arguments."
  (setf (copilot-chat-function-arguments function)
        (concat (copilot-chat-function-arguments function) args)))

(defun copilot-chat--append-vector-to-functions (vector functions)
  "Create or update FUNCTIONS elements using tool call data from VECTOR."
  (let ((name "")
        (args "")
        (id "")
        (index -1))

    (dolist (item (elt vector 0))
      (cond
       ((eq (car item) 'function)
        (setq
         name (alist-get 'name item)
         args (alist-get 'arguments item)))
       ((eq (car item) 'id)
        (setq id (cdr item)))
       ((eq (car item) 'index)
        (setq index (cdr item)))))

    (when (>= index 0)
      (if (< index (length functions))
          (copilot-chat--update-function (nth index functions) args)
        (let ((new-function
               (make-copilot-chat-function :name name :arguments args :id id)))
          (setq functions (append functions (list new-function))))))
    functions))

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

(defun copilot-chat--send-function-result-if-needed
    (instance callback results functions)
  "Send the FUNCTIONS results if all calls are completed.
INSTANCE is the copilot chat instance.
CALLBACK is `copilot-chat--ask' callback.
RESULTS is the list of results collected.
ARGLIST is the list of arguments that were processed."
  (when (= (length results) (length functions))
    (copilot-chat--ask instance results callback)))


(defun copilot-chat--call-functions (instance functions callback)
  "Call the FUNCTIONS and manage the result.
INSTANCE is the copilot chat instance."
  (let ((results nil))
    (dolist (function functions)
      (let* ((connection (copilot-chat--mcp-find-connection instance function))
             (name (copilot-chat-function-name function))
             (arguments (copilot-chat-function-arguments function)))
        (if (yes-or-no-p
             (format
              "Copilot Chat wants to call the tool '%s' with arguments: %s. Allow?"
              name
              (if (string-empty-p arguments)
                  "none"
                arguments)))
            (mcp-async-call-tool
             connection name
             (unless (string-empty-p arguments)
               (json-parse-string arguments
                                  :object-type 'alist
                                  :false-object
                                  :json-false))
             (lambda (result)
               (push `(:role
                       "tool"
                       :tool_call_id ,(copilot-chat-function-id function)
                       :name ,name
                       :content
                       ,(plist-get (aref (plist-get result :content) 0) :text))
                     results)
               (copilot-chat--send-function-result-if-needed
                instance callback results functions))
             (lambda (_ msg)
               (message "Error calling function %s: %s" name msg)
               (push `(:role
                       "tool"
                       :tool_call_id ,(copilot-chat-function-id function)
                       :content ,msg)
                     results)
               (copilot-chat--send-function-result-if-needed
                instance callback results functions)))
          (push `(:role
                  "tool"
                  :tool_call_id ,(copilot-chat-function-id function)
                  :content ,(format "User denied the tool call for '%s'." name))
                results)
          (copilot-chat--send-function-result-if-needed
           instance callback results functions))))))

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
               (let* ((name (plist-get tool :name))
                      (desc (plist-get tool :description))
                      (type (plist-get (plist-get tool :inputSchema) :type))
                      (properties
                       (plist-get (plist-get tool :inputSchema) :properties)))
                 (push `(:type
                         "function"
                         :function
                         (:name
                          ,(or name "")
                          :description ,(or desc "")
                          :parameters
                          (:type
                           ,(or type "object")
                           :properties ,(or properties (list)))))
                       all-tools)))
             tools)))))
    all-tools))

(provide 'copilot-chat-mcp)
;;; copilot-chat-mcp.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; checkdoc-verb-check-experimental-flag: nil
;; End:
