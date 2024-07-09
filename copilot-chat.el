(require 'json)
(require 'url)

;; Fonctions utilitaires

(defun copilot-chat-uuid ()
  "Generate a UUID."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (random 16384) 16384)
          (logior (random 4096) 32768)
          (random 65536) (random 65536) (random 65536)))

(defun copilot-chat-machine-id ()
  "Generate a machine ID."
  (let ((hex-chars "0123456789abcdef")
        (length 65)
        (hex ""))
    (dotimes (_ length)
      (setq hex (concat hex (string (aref hex-chars (random 16))))))
    hex))

(defun copilot-chat-get-cached-token ()
  "Get the cached GitHub token."
  (or (getenv "GITHUB_TOKEN")
	  "your github token"
      ))

(defconst copilot-chat-prompts
  '((explain . "Please write an explanation for the following code:\n")
	(review . "Please review the following code:\n")
	(doc . "Please write documentation for the following code:\n")
	(fix . "There is a problem in this code. Please rewrite the code to show it with the bug fixed.\n")
	(optimize . "Please optimize the following code to improve performance and readablilty:\n")
	(test . "Please generate tests for the followifg code:\n"))
    "Copilot chat predefined prompts")

(cl-defstruct copilot-chat
  github-token
  token
  sessionid
  machineid
  authinfo
)


(defvar copilot-chat-instance
  (make-copilot-chat
   :github-token (copilot-chat-get-cached-token)
   :token nil
   :sessionid nil
   :machineid (copilot-chat-machine-id)
   :authinfo nil
   ))

(defvar copilot-chat-buffer (get-buffer-create "*Copilot-chat*"))


(defvar copilot-chat-prompt
  "You are a world-class coding tutor. Your code explanations perfectly balance high-level concepts and granular details. Your approach ensures that students
 not only understand how to write code, but also grasp the underlying principles that guide effective programming.\nWhen asked for your name, you must respond with \"GitHub Copilot\".\nFollow the user's requirements carefully & to the letter.\nYour expertise is strictly limited to software development topics.\nFollow M
icrosoft content policies.\nAvoid content that violates copyrights.\nFor questions not related to software development, simply give a reminder that you are an AI programming assistant.\nKeep your answers short and impersonal.\nUse Markdown formatting in your answers.\nMake sure to include the programming language name 
at the start of the Markdown code blocks.\nAvoid wrapping the whole response in triple backticks.\nThe user works in an IDE called Neovim which has a concept for editors with open files, integrated unit test support, an output pane that shows the output of running the code as well as an integrated terminal.\nThe active
 document is the source code the user is looking at right now.\nYou can only give one reply for each conversation turn.\n\nAdditional Rules\nThink step by step:\n1. Examine the provided code selection and any other context like user question, related errors, project details, class definitions, etc.\n2. If you are unsur
e about the code, concepts, or the user's question, ask clarifying questions.\n3. If the user provided a specific question or error, answer it based on the selected code and additional provided context. Otherwise focus on explaining the selected code.\n4. Provide suggestions if you see opportunities to improve code rea
dability, performance, etc.\n\nFocus on being clear, helpful, and thorough without assuming extensive prior knowledge.\nUse developer-friendly terms and analogies in your explanations.\nIdentify 'gotchas' or less obvious parts of the code that might trip up someone new.\nProvide clear and relevant examples aligned with
 any provided context.\n")

(defun copilot-chat-create (&optional proxy allow-insecure)
  (interactive)
  "Create a new Copilot chat instance."
  (setq copilot-chat-instance(make-copilot-chat
							  :github-token (copilot-chat-get-cached-token)
							  :token nil
							  :sessionid nil
							  :machineid (copilot-chat-machine-id))))

(defun copilot-chat-create-req(prompt)
  "Create a request for Copilot."
  (json-encode `(("messages" . [,(list (cons "content" copilot-chat-prompt) (cons "role" "system"))
								,(list (cons "content" prompt) (cons "role" "user"))])
				 ("top_p" . 1)
				 ("model" . "gpt-4")
				 ("stream" . t)
				 ("n" . 1)
				 ("intent" . t)
				 ("temperature" . 0.1))))



(defun copilot-chat-auth-cb(status callback &optional CBARGS)
;  (if (plist-get status :error)
 ;     (message "Authentication error : %s" (plist-get status :error))
	(beginning-of-buffer)
	(switch-to-buffer (current-buffer))
    (re-search-forward "\n\n")
	(let ((json-object-type 'alist)
          (json-array-type 'list))
      (setf (copilot-chat-token copilot-chat-instance) (json-read))
      (setf (copilot-chat-sessionid copilot-chat-instance) (copilot-chat-uuid)))
	(funcall callback CBARGS))

(defun copilot-chat-auth(callback &optional CBARGS)
  "Authenticate with GitHub Copilot API."
  (if (null (copilot-chat-github-token copilot-chat-instance))
	  (error "No GitHub token found")
	(if (or (null (copilot-chat-token copilot-chat-instance))
			(> (round (float-time (current-time))) (alist-get 'expires_at (copilot-chat-token copilot-chat-instance))))
		(let ((url "https://api.github.com/copilot_internal/v2/token")
			  (url-request-method "GET")
			  (url-request-extra-headers `(("authorization" . ,(concat "token " (copilot-chat-github-token copilot-chat-instance)))
										   ("accept" . "application/json")
										   ("editor-version" . "Neovim/0.10.0")
										   ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
										   ("user-agent" . "CopilotChat.nvim/2.0.0"))))
;		  (switch-to-buffer (url-retrieve url 'copilot-chat-auth-cb '('callback
;CBARGS))))
		  (url-retrieve url 'copilot-chat-auth-cb '('callback CBARGS)))
	  (message "Already authenticated with GitHub Copilot API")
	  (funcall callback CBARGS))))


(defun analyze-copilot-response (status)
  "Analyse Copilot answer in current buffer."
  (if (plist-get status :error)
      (message "Copilot request error : %s" (plist-get status :error))

	(while (re-search-forward "^data: " nil t)
      (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
			 (json-string (and (not (string= "[DONE]" line)) line))
			 (json-obj (and json-string (json-parse-string json-string :object-type 'alist)))
			 (choices (and json-obj (alist-get 'choices json-obj)))
			 (delta (and (> (length choices) 0) (alist-get 'delta (aref choices 0))))
			 (token (and delta (alist-get 'content delta))))
		(when (and token (not (eq token :null)))
		  (with-current-buffer copilot-chat-buffer
			(insert token)))))
	(when (and copilot-chat-buffer (not (get-buffer-window copilot-chat-buffer 'visible)))
      (switch-to-buffer copilot-chat-buffer))))


(defun copilot-chat-ask-cb (prompt)
  (let ((url "https://api.githubcopilot.com/chat/completions")
		(url-request-method "POST")
		(url-mime-encoding-string nil)
		(url-user-agent "CopilotChat.nvim/2.0.0")
		(url-http-attempt-keepalives nil)
		(url-request-extra-headers `(("openai-intent" . "conversation-panel")
									 ("content-type" . "application/json")
									 ("editor-plugin-version" . "CopilotChat.nvim/2.0.0")
									 ("authorization" . ,(concat "Bearer "
																 (alist-get 'token (copilot-chat-token copilot-chat-instance))))
									 ("x-request-id" . ,(copilot-chat-uuid))
									 ("vscode-sessionid" . ,(copilot-chat-sessionid copilot-chat-instance))
									 ("vscode-machineid" . ,(copilot-chat-machineid copilot-chat-instance))
									 ("copilot-integration-id" . "vscode-chat")
									 ("openai-organization" . "github-copilot")
									 ("editor-version" . "Neovim/0.10.0")))
		(url-request-data (copilot-chat-create-req prompt)))
	(url-retrieve url 'analyze-copilot-response)))


(defun copilot-chat-ask (prompt)
  "Ask a question to Copilot."
  (with-current-buffer copilot-chat-buffer
			   (erase-buffer)
			   (insert (concat prompt "\n\n###")))
  (copilot-chat-auth 'copilot-chat-ask-cb prompt))


(defun copilot-ask-region(prompt)
  (copilot-chat-ask (concat (cdr (assoc prompt copilot-chat-prompts)) (buffer-substring-no-properties (region-beginning) (region-end)))))

(defun copilot-chat-explain()
  "Ask Copilot to explain the current selected code."
  (interactive)
  (copilot-ask-region 'explain))

(defun copilot-chat-review()
  "Ask Copilot to review the current selected code."
  (interactive)
  (copilot-ask-region 'review))

(defun copilot-chat-doc()
  "Ask Copilot to write documentation for the current selected code."
  (interactive)
  (copilot-ask-region 'doc))

(defun copilot-chat-fix()
  "Ask Copilot to fix the current selected code."
  (interactive)
  (copilot-ask-region 'fix))

(defun copilot-chat-optimize()
  "Ask Copilot to optimize the current selected code."
  (interactive)
  (copilot-ask-region 'optimize))

(defun copilot-chat-test()
  "Ask Copilot to generate tests for the current selected code."
  (interactive)
  (copilot-ask-region 'test))
