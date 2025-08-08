;;; copilot-chat --- copilot-chat-curl.el --- copilot chat curl backend -*- lexical-binding: t; -*-

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
;; This is curl backend for copilot-chat code

;;; Code:

(require 'copilot-chat-body)
(require 'copilot-chat-common)
(require 'copilot-chat-connection)
(require 'copilot-chat-spinner)
(require 'copilot-chat-backend)
(require 'copilot-chat-mcp)

;; customs
(defcustom copilot-chat-curl-program "curl"
  "Curl program to use if `copilot-chat-use-curl' is set."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-curl-proxy nil
  "Curl will use this proxy if defined.
The proxy string can be specified with a protocol:// prefix.  No protocol
specified or http:// it is treated as an HTTP proxy.  Use socks4://,
socks4a://, socks5:// or socks5h:// to request a specific SOCKS version
to be used.

Unix domain sockets are supported for socks proxy.  Set localhost for the
host part.  e.g. socks5h://localhost/path/to/socket.sock

HTTPS proxy support works set with the https:// protocol prefix for
OpenSSL and GnuTLS.  It also works for BearSSL, mbedTLS, rustls,
Schannel, Secure Transport and wolfSSL (added in 7.87.0).

Unrecognized and unsupported proxy protocols cause an error.  Ancient
curl versions ignored unknown schemes and used http:// instead.

If the port number is not specified in the proxy string, it is assumed
to be 1080.

This option overrides existing environment variables that set the proxy
to use.  If there is an environment variable setting a proxy, you can set
proxy to \"\" to override it.

User and password that might be provided in the proxy string are URL
decoded by curl. This allows you to pass in special characters such as @
by using %40 or pass in a colon with %3a."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-curl-proxy-insecure nil
  "Insecure flag for `copilot-chat' proxy with curl backend.
Every secure connection curl makes is verified to be secure before the
transfer takes place.  This option makes curl skip the verification step
with a proxy and proceed without checking."
  :type 'boolean
  :group 'copilot-chat)

(defcustom copilot-chat-curl-proxy-user-pass nil
  "User password for `copilot-chat' proxy with curl backend.
Specify the username and password <user:password> to use for proxy
authentication."
  :type 'boolean
  :group 'copilot-chat)

;; structures
(cl-defstruct
 copilot-chat-curl
 "Private data for Copilot chat curl backend."
 (answer nil :type (or null string))
 (function nil :type (or null copilot-chat-function))
 (file nil :type (or null file))
 (current-data nil :type (or null string))
 (process nil :type (or null process)))


;; functions
(defun copilot-chat--curl-call-process (address method data &rest args)
  "Call curl synchronously.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Arguments ARGS are additional arguments to pass to curl."
  (let ((curl-args
         (append
          (list
           address
           "-s"
           "-X"
           (if (eq method 'post)
               "POST"
             "GET")
           "-A"
           "user-agent: CopilotChat.nvim/2.0.0"
           "-H"
           "content-type: application/json"
           "-H"
           "accept: application/json"
           "-H"
           "editor-plugin-version: CopilotChat.nvim/2.0.0"
           "-H"
           "editor-version: Neovim/0.10.0")
          (when data
            (list "-d" data))
          (when copilot-chat-curl-proxy
            (list "-x" copilot-chat-curl-proxy))
          (when copilot-chat-curl-proxy-insecure
            (list "--proxy-insecure"))
          (when copilot-chat-curl-proxy-user-pass
            (list "-U" copilot-chat-curl-proxy-user-pass))
          args)))
    (let ((result
           (apply #'call-process
                  copilot-chat-curl-program
                  nil
                  t
                  nil
                  curl-args)))
      (when (/= result 0)
        (error (format "curl returned non-zero result: %d" result))))))

(defun copilot-chat--curl-make-process
    (instance address method data filter vision callback &rest args)
  "Call curl asynchronously for INSTANCE.
Argument ADDRESS is the URL to call.
Argument METHOD is the HTTP method to use.
Argument DATA is the data to send.
Argument FILTER is the function called to parse data.
If VISION is t, add vision header.
Argument CALLBACK is the function to call with analysed data.
Optional argument ARGS are additional arguments to pass to curl."
  (let ((command
         (append
          (list
           copilot-chat-curl-program
           address
           "-s"
           "-X"
           (if (eq method 'post)
               "POST"
             "GET")
           "-A"
           "user-agent: CopilotChat.nvim/2.0.0"
           "-H"
           "content-type: application/json"
           "-H"
           "accept: application/json"
           "-H"
           "editor-plugin-version: CopilotChat.nvim/2.0.0"
           "-H"
           "editor-version: Neovim/0.10.0"
           "-H"
           "copilot-integration-id: vscode-chat")
          (when vision
            (list "-H" "Copilot-Vision-Request: true"))
          (when data
            (list "-d" data))
          (when copilot-chat-curl-proxy
            (list "-x" copilot-chat-curl-proxy))
          (when copilot-chat-curl-proxy-insecure
            (list "--proxy-insecure"))
          (when copilot-chat-curl-proxy-user-pass
            (list "-U" copilot-chat-curl-proxy-user-pass))
          args)))
    (setf (copilot-chat-curl-process (copilot-chat--backend instance))
          (make-process
           :name "copilot-chat-curl"
           :buffer nil
           :filter filter
           :sentinel
           (lambda (proc _exit)
             (when (/= (process-exit-status proc) 0)
               (let ((error-msg
                      (format "Curl interrupted: %d"
                              (process-exit-status proc))))
                 (funcall callback instance error-msg)
                 (funcall callback instance copilot-chat--magic)))
             (setf (copilot-chat-curl-process (copilot-chat--backend instance))
                   nil)
             (copilot-chat--spinner-stop instance))
           :stderr (get-buffer-create "*copilot-chat-curl-stderr*")
           :command command))))

(defun copilot-chat--curl-parse-github-token ()
  "Curl github token request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer :false-object :json-false))
         (token (gethash "access_token" json-data))
         (token-dir
          (file-name-directory
           (expand-file-name copilot-chat-github-token-file))))
    (setf (copilot-chat-connection-github-token copilot-chat--connection) token)
    (when (not (file-directory-p token-dir))
      (make-directory token-dir t))
    (with-temp-file copilot-chat-github-token-file
      (insert token))))

(defun copilot-chat--curl-parse-login ()
  "Curl login request parsing."
  (goto-char (point-min))
  (let* ((json-data (json-parse-buffer :false-object :json-false))
         (device-code (gethash "device_code" json-data))
         (user-code (gethash "user_code" json-data))
         (verification-uri (gethash "verification_uri" json-data)))
    (gui-set-selection 'CLIPBOARD user-code)
    (read-from-minibuffer
     (format
      "Your one-time code %s is copied. \
Press ENTER to open GitHub in your browser. \
If your browser does not open automatically, browse to %s."
      user-code verification-uri))
    (browse-url verification-uri)
    (read-from-minibuffer "Press ENTER after authorizing.")
    (with-temp-buffer
      (copilot-chat--curl-call-process
       "https://github.com/login/oauth/access_token" 'post
       (format
        "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"device_code\":\"%s\",\"grant_type\":\"urn:ietf:params:oauth:grant-type:device_code\"}"
        device-code))
      (copilot-chat--curl-parse-github-token))))


(defun copilot-chat--curl-login ()
  "Manage github login."
  (with-temp-buffer
    (copilot-chat--curl-call-process
     "https://github.com/login/device/code"
     'post
     "{\"client_id\":\"Iv1.b507a08c87ecfe98\",\"scope\":\"read:user\"}")
    (copilot-chat--curl-parse-login)))


(defun copilot-chat--curl-parse-renew-token ()
  "Curl renew token request parsing."
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (let ((json-data
         (json-parse-buffer
          :object-type 'alist ;need alist to be compatible with
          ;copilot-chat-token format
          :false-object
          :json-false))
        (cache-dir
         (file-name-directory (expand-file-name copilot-chat-token-cache))))
    (setf (copilot-chat-connection-token copilot-chat--connection) json-data)
    ;; save token in copilot-chat-token-cache file after creating
    ;; folders if needed
    (when (not (file-directory-p cache-dir))
      (make-directory cache-dir t))
    (with-temp-file copilot-chat-token-cache
      (insert (json-serialize json-data :false-object :json-false)))))


(defun copilot-chat--curl-renew-token ()
  "Renew session token."
  (with-temp-buffer
    (copilot-chat--curl-call-process
     "https://api.github.com/copilot_internal/v2/token" 'get nil
     "-H"
     (format "authorization: token %s"
             (copilot-chat-connection-github-token copilot-chat--connection)))
    (copilot-chat--curl-parse-renew-token)))


(defun copilot-chat--curl-extract-segment (segment)
  "Extract data from an individual line-delimited SEGMENT, returning one of:
- `empty` if the segment has no data
- `partial`: if the segment seems to be incomplete, i.e. more data in a
  future response
- `done`: if this segment indicates completion (data: [DONE])
- otherwise, the entire JSON content (data: {...})
Argument SEGMENT is data segment to parse."
  (cond
   ;; empty
   ((string-empty-p segment)
    'empty)
   ;; seems to have a valid prefix
   ((string-prefix-p "data: " segment)
    (let ((data (substring segment 6)))
      (if (string= data "[DONE]")
          ;; the magic done marker
          'done
        ;; not the done marker, so must be "done: {...json...}"
        (condition-case _err
            (json-parse-string data
                               :object-type 'alist
                               :false-object
                               :json-false)
          ;; failure => the segment was probably truncated and we need more data from a future
          ;; response
          (json-parse-error
           'partial)
          (json-end-of-file
           'partial)))))
   ;; otherwise, try parsing the segment as a non-prefixed json (such as in
   ;; error responses) When even this fails, then we have a partial response
   ;; that was probably truncated (e.g. "dat", or "data:") => need more data
   ;; from a future response
   (t
    (condition-case _err
        (json-parse-string segment
                           :object-type 'alist
                           :false-object
                           :json-false)
      (error
       'partial)))))


(defun copilot-chat--curl-analyze-response (instance string callback no-history)
  "Analyse curl response.
Argument INSTANCE is the copilot chat instance to use.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
if the response should be added to history."
  ;; The API conceptually sends us big blob of line-deliminated information, e.g.
  ;;
  ;;     data: {"choices":[{...,"delta":{"content":"great"}}],...}
  ;;
  ;;     data: {"choices":[{...,"delta":{"content":"work"}}],...}
  ;;
  ;;     data: [DONE]
  ;;
  ;; We receive this piecewise, with this function called with `string' as any substring, completely
  ;; ignoring the lines and other rules of the protocol. Thus, this function processes line-by-line
  ;; but needs to be careful to handle partial input any point. We do this by saving a left-over
  ;; line that failed processing to `curl-current-data' and reading it on the next call.
  ;;
  ;; For instance, this function could be called with three segments like:
  ;;
  ;; 1. "data: {...}\n\ndat" (break in the middle of a "data: " prefix)
  ;; 2. "a: {...}\n\ndata: [D" (break in the middle of some data content)
  ;; 3. "ONE]\n\n"
  ;;
  ;; Those calls will proceed like this:
  ;;
  ;; 1. With segment 1, successfully process the first line (`callback' is called with argument "great"), skip
  ;;    the next empty line, and then fail to process the trailing "dat"; "dat" is saved to
  ;;    `curl-current-data'.
  ;;
  ;; 2. With segment 2, the value of `curl-current-data' is first prepended to `string', and
  ;;    processing continues with "data: {...}\n\ndata: [D". Thus, `callback' is called with "work",
  ;;    the next line skipped, and then "data: [D" saved to `curl-current-data'.
  ;;
  ;; 3. With segment 3, `curl-current-data' is prepended to `string', resulting in a value of
  ;;    "data: [DONE]\n\n". Thus, `callback' is called with the value of `copilot-chat--magic', and
  ;;    the two trailing empty lines are skipped.
  (when (copilot-chat-curl-current-data (copilot-chat--backend instance))
    (setq string
          (concat
           (copilot-chat-curl-current-data
            (copilot-chat--backend instance))
           string))
    (setf (copilot-chat-curl-current-data (copilot-chat--backend instance))
          nil))

  (let ((segments (split-string string "\n")))
    (dolist (segment segments)
      (let ((extracted (copilot-chat--curl-extract-segment segment)))
        (cond
         ;; No data at all, just skip:
         ((eq extracted 'empty)
          nil)
         ;; Data looks truncated, save it for the next segment:
         ((eq extracted 'partial)
          (setf (copilot-chat-curl-current-data
                 (copilot-chat--backend instance))
                segment))
         ;; Final segment, all done:
         ((eq extracted 'done)
          (let ((answer
                 (copilot-chat-curl-answer (copilot-chat--backend instance)))
                (function
                 (copilot-chat-curl-function (copilot-chat--backend instance))))

            ;; History
            (unless no-history
              (let ((new-hist
                     `(:content
                       ,(if answer
                            answer
                          "")
                       :role "assistant")))
                (unless (string-empty-p (copilot-chat-function-name function))
                  (setq new-hist
                        (append
                         new-hist
                         `(:tool_calls
                           ,(vconcat
                             `((:type
                                "function"
                                :id
                                ,(copilot-chat-function-id function)
                                :function
                                (:name
                                 ,(copilot-chat-function-name function)
                                 :arguments
                                 ,(copilot-chat-function-arguments
                                   function)))))))))
                (setf (copilot-chat-history instance)
                      (cons new-hist (copilot-chat-history instance)))))

            ;; manage tool
            (if (not (string-empty-p (copilot-chat-function-name function)))
                ;; We have a tool to call
                (copilot-chat--call-function instance function callback)

              ;; Else, we are not using a tool,
              ;; so just we can send magic and clean.
              (copilot-chat--spinner-stop instance)
              (funcall callback instance copilot-chat--magic))
            (setf
             (copilot-chat-curl-function (copilot-chat--backend instance))
             (make-copilot-chat-function)
             (copilot-chat-curl-answer (copilot-chat--backend instance)) nil)))

         ;; Otherwise, JSON parsed successfully
         (extracted
          (cond
           ;; extract .choices[0].delta.content and pass that along:
           ((alist-get 'choices extracted)
            (let* ((choices (alist-get 'choices extracted))
                   (delta
                    (and (> (length choices) 0)
                         (alist-get 'delta (aref choices 0))))
                   (token (and delta (alist-get 'content delta))))
              (when token
                (if (eq token :null)
                    (let ((tool_calls (alist-get 'tool_calls delta)))
                      (when (and tool_calls (not (eq tool_calls :null)))
                        (copilot-chat--append-vector-to-function
                         tool_calls
                         (copilot-chat-curl-function
                          (copilot-chat--backend instance)))))
                  (when (not
                         (copilot-chat-curl-answer
                          (copilot-chat--backend instance)))
                    (copilot-chat--spinner-set-status instance "Generating"))
                  (funcall callback instance token)
                  (setf (copilot-chat-curl-answer
                         (copilot-chat--backend instance))
                        (concat
                         (copilot-chat-curl-answer
                          (copilot-chat--backend instance))
                         token))))))

           ;; display .error.message in the chat.
           ((alist-get 'error extracted)
            (copilot-chat--spinner-stop instance)
            (let* ((err-response (alist-get 'error extracted))
                   (err-message (alist-get 'message err-response))
                   (answer (format "Error: %s" err-message)))
              (message answer)
              (funcall callback instance answer)
              (funcall callback instance copilot-chat--magic)
              ;; Add an empty response to the chat history to avoid confusing
              ;; the assistant with its own error messages...
              (setf
               (copilot-chat-history instance)
               (cons
                `(:content "" :role "assistant")
                (copilot-chat-history instance))
               (copilot-chat-curl-answer (copilot-chat--backend instance)) nil
               (copilot-chat-curl-function (copilot-chat--backend instance)) (make-copilot-chat-function))))
           ;; Fallback -- nag developers about possibly unhandled payloads
           (t
            (warn "Unhandled message from copilot: %S" extracted)))))))))

(defun copilot-chat--curl-analyze-nonstream-response
    (instance proc string callback no-history)
  "Analyse curl response non stream version.
o1 differs from the other models in the format of the reply.
Argument INSTANCE is the copilot chat instance to use.
Argument PROC is curl process.
Argument STRING is the data returned by curl.
Argument CALLBACK is the function to call with analysed data.
Argument NO-HISTORY is a boolean to indicate
 if the response should be added to history."
  (when (copilot-chat-curl-current-data (copilot-chat--backend instance))
    (setq string
          (concat
           (copilot-chat-curl-current-data
            (copilot-chat--backend instance))
           string))
    (setf (copilot-chat-curl-current-data (copilot-chat--backend instance))
          nil))

  (condition-case err
      (let* ((extracted
              (json-parse-string string
                                 :object-type 'alist
                                 :false-object
                                 :json-false))
             (choices (alist-get 'choices extracted))
             (message
              (and (> (length choices) 0)
                   (alist-get 'message (aref choices 0))))
             (token (and message (alist-get 'content message))))
        (when (and token (not (eq token :null)))
          (copilot-chat--spinner-stop instance)
          (funcall callback instance token)
          (funcall callback instance copilot-chat--magic)
          (setf (copilot-chat-curl-answer (copilot-chat--backend instance))
                (concat
                 (copilot-chat-curl-answer
                  (copilot-chat--backend instance))
                 token))
          (unless no-history
            (setf (copilot-chat-history instance)
                  (cons
                   `(:content
                     ,(copilot-chat-curl-answer
                       (copilot-chat--backend instance))
                     :role "assistant")
                   (copilot-chat-history instance))))
          (setf
           (copilot-chat-curl-answer (copilot-chat--backend instance)) nil
           (copilot-chat-curl-function (copilot-chat--backend instance)) (make-copilot-chat-function)
           (copilot-chat-curl-current-data (copilot-chat--backend instance)) nil)))
    ;; o1 often returns `rate limit exceeded` because of its severe rate limitation,
    ;; so the message in case of an error should be easy to understand.
    (error
     ;; When JSON parsing fails, but the process has not terminated and may be
     ;; in the middle of a sentence, do not make an error, set the string and
     ;; wait for the next call.
     ;; I'm not sure if asynchronous control is working properly.
     (progn
       (setf (copilot-chat-curl-current-data (copilot-chat--backend instance))
             string)
       (unless (process-live-p proc)
         (copilot-chat--spinner-stop instance)
         (setf (copilot-chat-curl-current-data (copilot-chat--backend instance))
               nil)
         (funcall callback
                  instance
                  (format "GitHub Copilot error: %S\nResponse is %S"
                          err
                          (string-trim string))))))))

(defun copilot-chat--curl-ask (instance prompt callback out-of-context)
  "Ask a question to Copilot using curl backend.
Argument INSTANCE is the copilot chat instance to use.
Argument PROMPT is the prompt to send to copilot.  It can be a string or a list
of json objects.
Argument CALLBACK is the function to call with copilot answer as argument.
Argument OUT-OF-CONTEXT is a boolean to indicate
if the prompt is out of context."
  (setf
   (copilot-chat-curl-current-data (copilot-chat--backend instance)) nil
   (copilot-chat-curl-function (copilot-chat--backend instance)) (make-copilot-chat-function)
   (copilot-chat-curl-answer (copilot-chat--backend instance)) nil)

  ;; Start the spinner animation only for instances with chat buffers
  (when (buffer-live-p (copilot-chat-chat-buffer instance))
    (copilot-chat--spinner-start instance))

  (let ((file (copilot-chat-curl-file (copilot-chat--backend instance))))
    (when (and file (file-exists-p file))
      (delete-file file)))
  (setf (copilot-chat-curl-file (copilot-chat--backend instance))
        (make-temp-file "copilot-chat"))
  (let ((coding-system-for-write 'raw-text))
    (with-temp-file (copilot-chat-curl-file (copilot-chat--backend instance))
      (insert (copilot-chat--create-req instance prompt out-of-context))))

  (unless out-of-context
    (let* ((history (copilot-chat-history instance))
           (new-history
            (if (stringp prompt)
                ;; classic prompt
                (cons `(:content ,prompt :role "user") history)
              ;; tool answer
              (append (list prompt) history))))
      (setf (copilot-chat-history instance) new-history)))

  (copilot-chat--curl-make-process
   instance
   "https://api.githubcopilot.com/chat/completions"
   'post
   (concat "@" (copilot-chat-curl-file (copilot-chat--backend instance)))
   (lambda (proc string)
     (copilot-chat--debug 'curl "copilot-chat--curl-ask: %s" string)
     (if (not
          (string= string "quota exceeded\n"))
         (if (copilot-chat--instance-support-streaming instance)
             (copilot-chat--curl-analyze-response
              instance string callback out-of-context)
           (copilot-chat--curl-analyze-nonstream-response
            instance proc string callback out-of-context))
       (copilot-chat--spinner-stop instance)
       (funcall callback instance "Quota exceeded.")))
   (copilot-chat-uses-vision instance)
   callback
   "-H"
   "openai-intent: conversation-panel"
   "-H"
   (concat
    "authorization: Bearer "
    (alist-get 'token (copilot-chat-connection-token copilot-chat--connection)))
   "-H"
   (concat "x-request-id: " (copilot-chat--uuid))
   "-H"
   (concat
    "vscode-sessionid: "
    (copilot-chat-connection-sessionid copilot-chat--connection))
   "-H"
   (concat
    "vscode-machineid: "
    (copilot-chat-connection-machineid copilot-chat--connection))
   "-H"
   "copilot-integration-id: vscode-chat"))

(defun copilot-chat--curl-cancel (instance)
  "Cancel the current request for INSTANCE."
  (copilot-chat--spinner-stop instance)
  (let ((proc (copilot-chat-curl-process (copilot-chat--backend instance))))
    (when (process-live-p proc)
      (delete-process proc))))

(defun copilot-chat--curl-quotas ()
  "Get the current GitHub Copilot quotas."
  (with-temp-buffer
    (let* ((curl-args
            (append
             (list
              "https://api.github.com/rate_limit" "-s" "-X" "GET" "-H"
              (concat
               "authorization: Bearer "
               (copilot-chat-connection-github-token copilot-chat--connection))
              "-H" "Accept: application/vnd.github+json")))
           (result
            (apply #'call-process
                   copilot-chat-curl-program
                   nil
                   t
                   nil
                   curl-args)))
      (when (/= result 0)
        (error (format "curl returned non-zero result: %d" result))))
    (goto-char (point-min))
    (let* ((json-data
            (json-parse-buffer :object-type 'alist :false-object :json-false))
           (resources (alist-get 'resources json-data))
           (result '()))
      (dolist (resource resources)
        (let* ((name
                (capitalize
                 (replace-regexp-in-string
                  "_" " "
                  (symbol-name (car resource)))))
               (data (cdr resource))
               (limit (alist-get 'limit data))
               (used (alist-get 'used data))
               (remaining (alist-get 'remaining data))
               (reset (alist-get 'reset data)))
          (push (list name limit used remaining reset) result)))
      (nreverse result))))

(defun copilot-chat--curl-init (instance)
  "Initialize Copilot chat curl backend for INSTANCE."
  (setf (copilot-chat--backend instance) (make-copilot-chat-curl)))


;; Top-level execute code.
(cl-pushnew
 (make-copilot-chat-backend
  :id 'curl
  :init-fn #'copilot-chat--curl-init
  :clean-fn nil
  :login-fn #'copilot-chat--curl-login
  :renew-token-fn #'copilot-chat--curl-renew-token
  :ask-fn #'copilot-chat--curl-ask
  :cancel-fn #'copilot-chat--curl-cancel
  :quotas-fn #'copilot-chat--curl-quotas)
 copilot-chat--backend-list
 :test #'equal)

(provide 'copilot-chat-curl)
;;; copilot-chat-curl.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
