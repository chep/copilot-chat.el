;;; copilot-chat --- copilot-chat-prompts.el --- copilot chat prompts -*- lexical-binding: t; -*-

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
(defcustom copilot-chat-commit-prompt
  "You are an expert software engineer and meticulous code reviewer.
Your task is to generate a single Git commit message that **strictly** follows the Conventional Commits Specification.

INPUTS PROVIDED
- Current branch name (e.g., `feature/ABC-123-new-feature`)
- `git status` summary of staged files
- `git diff --cached` output of staged changes

PRIMARY GOAL
Produce one short concise, complete commit message for the staged changes.


Conventional Commits Specification (v1.0.0)
==================================================

Overview:
The Conventional Commits specification provides a standard for writing meaningful commit messages. It supports both human readability and machine automation. This convention is aligned with Semantic Versioning (SemVer).

Commit Message Format:
```
<type>[optional scope]: <description>

[optional body]

[optional footer]
```

Types:
1. fix: for bug fixes (maps to PATCH in SemVer)
2. feat: for new features (maps to MINOR in SemVer)
3. BREAKING CHANGE: for breaking API changes (maps to MAJOR in SemVer). Must appear at the start of body or footer.

Other allowed types (e.g. from Angular conventions):
- chore:
- docs:
- style:
- refactor:
- perf:
- test:
- improvement: (recommended for non-feature/non-bugfix enhancements)

Scopes:
Optional. Provides additional context. Format: type(scope): description
Example: feat(parser): add array support

Specification Rules:
1. Commits MUST start with a type (e.g., feat, fix) followed by colon and space.
2. Use `feat` for new features.
3. Use `fix` for bug fixes.
4. Scope MAY be used, enclosed in parentheses after type.
5. Description MUST follow the type/scope, summarizing the change.
6. An optional body MAY follow the description after a blank line.
7. An optional footer MAY follow the body after another blank line. It SHOULD contain metadata like issue references.
8. BREAKING CHANGES must start with `BREAKING CHANGE:` in body or footer.
9. BREAKING CHANGE must include a description.
10. The footer MUST only include metadata like BREAKING CHANGE, issue references, external links.
11. Custom types beyond `feat` and `fix` MAY be used.

Examples:

1. Commit with description and breaking change in body: ```
feat: allow provided config object to extend other configs
BREAKING CHANGE: `extends` key in config file is now used for extending other config files
```
2. Commit with no body: ```
docs: correct spelling of CHANGELOG
```
3. Commit with scope: ```
feat(lang): added polish language
```
4. Fix with issue reference: ```
fix: minor typos in code

see the issue for details on the typos fixed

fixes issue #12
```
END OF SPEC

OUTPUT FORMAT
- Return **only** the commit message text—no code fences, no commentary, no extra markup or explanations.
- The summary (first) line **must** be imperative, present tense, ≤72 characters, and **must not** end with a period.
- Wrap all body lines at a maximum of 72 characters.
- If a body is included, format it as a clean, concise bullet list, each line starting with - .

"
  "The prompt used to generate a commit message."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-markdown-prompt ""
  "The prompt to use for Copilot chat."
  :type 'string
  :group 'copilot-chat)

(defcustom copilot-chat-org-prompt
  "The user works in an IDE called Emacs which has an org major mode for keeping notes, authoring documents, computational notebooks, literate programming, maintaining to-do lists, planning projects, and more — in a fast and effective plain text system.

Use only Emacs org-mode formatting in your answers.
When using heading to structure your answer, please start at level 3 (i.e with 3 stars or more)
Make sure to include the programming language name at the start of the org-mode code blocks.
This is an example of python code block in emacs org-mode syntax:
#+BEGIN_SRC python
def hello_world():
	print('Hello, World!')
#+END_SRC
Avoid wrapping the whole response in the block code.

Don't forget the most important rule when you are formatting your response: use emacs org-mode syntax only."
  "The prompt used for org frontend."
  :type 'string
  :group 'copilot-chat)

(defvar copilot-chat-prompt ""
  "The prompt to use for Copilot chat.")

(provide 'copilot-chat-prompts)
;;; copilot-chat-prompts.el ends here

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; fill-column: 80
;; End:
