;;; copilot-chat --- copilot-chat-instance.el --- copilot chat instance management -*- lexical-binding: t; -*-

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
(require 'copilot-chat-common)

(defun copilot-chat--get-list-buffer-create (instance)
  "Get or create the Copilot chat list buffer for INSTANCE."
  (let ((list-buffer (get-buffer-create
                       (concat copilot-chat-list-buffer
                         "-"
                         (copilot-chat-directory instance)
                         "*"))))
    (with-current-buffer list-buffer
      (setq-local default-directory (copilot-chat-directory instance)))
    list-buffer))

;;;###autoload (autoload 'copilot-chat-kill-instance "copilot-chat" nil t)
(defun copilot-chat-kill-instance ()
  "Interactively kill a selected copilot chat instance.
All its associated buffers are killed."
  (interactive)
  (let* ( (instance (copilot-chat--choose-instance))
          (buf (copilot-chat--get-buffer instance))
          (lst-buf (copilot-chat--get-list-buffer-create instance))
          (tmp-buf (copilot-chat-shell-maker-tmp-buf instance)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (when (buffer-live-p lst-buf)
      (kill-buffer lst-buf))
    (when (buffer-live-p tmp-buf)
      (kill-buffer tmp-buf))
    (setq copilot-chat--instances (delete instance copilot-chat--instances))))


(defun copilot-chat--create-instance ()
  "Create a new copilot chat instance for a given directory."
  (let* ((current-dir (file-name-directory (or (buffer-file-name) default-directory)))
          (directory (expand-file-name
                       (read-directory-name "Choose a directory: " current-dir)))
          (found (copilot-chat--find-instance directory))
          (instance (if found
                      found
                      (copilot-chat--create directory))))
    (unless found
      (push instance copilot-chat--instances))
    instance))

(defun copilot-chat--find-instance (directory)
  "Find the instance corresponding to a path.
Argument DIRECTORY is the path to search for matching instance."
  (cl-find-if (lambda (instance)
                (string-prefix-p (copilot-chat-directory instance) directory))
    copilot-chat--instances))

(defun copilot-chat--ask-for-instance ()
  "Ask for an existing instance or create a new one."
  (if (null copilot-chat--instances)
    (copilot-chat--create-instance)
    (let* ((choice (read-multiple-choice
                     "Copilot Chat Instance: "
                     '((?c "Create new instance" "Create a new Copilot chat instance")
                        (?l "Choose from list" "Choose from existing instances"))))
            (key (car choice)))
      (cond
        ((eq key ?l) (copilot-chat--choose-instance))
        ((eq key ?c) (copilot-chat--create-instance))))))

(defun copilot-chat--current-instance ()
  "Return current instance, create a new one if needed."
  ;; check if we are in a copilot-chat buffer
  (let ((buf (pm-base-buffer)))
    ;; get file corresponding to buf
    ;; if no file, ask for an existing instanceor create a new one
    ;; if an instance as a parent path of the file, use it
    ;; else ask for an existing instance or create a new one
    (let* ( (parent (expand-file-name
                      (file-name-directory (or (buffer-file-name buf)
                                             default-directory))))
            (existing-instance (copilot-chat--find-instance parent)))
      (if existing-instance
        existing-instance
        (copilot-chat--ask-for-instance)))))

(defun copilot-chat--choose-instance ()
  "Choose an instance from the list of instances."
  ;; create a completion-choices list containing directory of all instances in
  ;; the copilot-chat--instances list. Get directory with
  ;; (copilot-chat-directory instance). Use completing-read to get user choice
  ;; and then use copilot-chat--find-instance to get corresponding instance
  (let* ((choices (mapcar (lambda (instance)
                            (cons (copilot-chat-directory instance) instance))
                    copilot-chat--instances))
          (choice (completing-read "Choose Copilot Chat instance: "
                    (mapcar 'car choices)
                    nil t)))
    (copilot-chat--find-instance choice)))

(defun copilot-chat--file-ignored-p (file ignore-files instance)
  "Return non-nil if FILE should be ignored.
Filter is based on IGNORE-FILES and the INSTANCE directory."
  (let* ((dir (copilot-chat-directory instance))
          ;; We use relative path so everything is expanded from the instance directory
          (relative-file (file-relative-name file dir)))
    (cl-some
      (lambda (pat)
        (cond
          ;; Check if pattern is a directory (ends with slash)
          ((string-suffix-p "/" pat)
            (string-prefix-p pat relative-file))
          ;; Otherwise interpret it as a globbing pattern or single filename
          (t (string-match-p (wildcard-to-regexp pat) relative-file))))
      ignore-files)))

(defun copilot-chat--add-workspace (instance)
  "Add all open files matching an instance to buffer list.
INSTANCE is the copilot chat instance to use."
  (copilot-chat--clear-buffers instance)
  ;; search for `.gitignore' in the directory of the instance
  ;; if found, use it to ignore files
  (let ( (gitignore (expand-file-name ".gitignore" (copilot-chat-directory instance)))
         (ignore-files '()))
    (when (file-exists-p gitignore)
      (with-temp-buffer
        (insert-file-contents gitignore)
        (setq ignore-files (split-string (buffer-string) "\n" t))))
    (dolist (buf (buffer-list))
      (let ((file (buffer-file-name buf)))
        (when (and file
                (string-prefix-p (copilot-chat-directory instance)
                  (file-name-directory file))
                (not (copilot-chat--file-ignored-p file ignore-files instance)))
          (copilot-chat--add-buffer instance buf))))))


(provide 'copilot-chat-instance)
;;; copilot-chat-instance.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; lisp-indent-offset: 2
;; package-lint-main-file: "copilot-chat.el"
;; End:
