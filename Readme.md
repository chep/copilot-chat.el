# Copilot Chat for Emacs
## Notes
This plugin is based on Copilot Chat for neovim repository: https://github.com/CopilotC-Nvim/CopilotChat.nvim

This is a work in progress. Feel free to help me. See in the [Todo list](#todo) if you can implement missing features.

## Installation
### Straight
```
(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
```
### With shell-maker

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :branch "shell-maker" :files ("*.el"))
  :after (shell-maker))

### Manual
Clone repository and eval files in emacs.

## Configuration
When sending the first prompt, you will need to authenticate to github. Follow instructions and everything will be fine.

## Usage
### Functions
- `(copilot-chat-display)` display copilot chat and prompt buffer.
- `(copilot-chat-explain)` ask copilot to explain selected code.
- `(copilot-chat-review)` ask copilot to review selected code.
- `(copilot-chat-doc)` ask copilot to document selected code.
- `(copilot-chat-fix)` ask copilot to fix selected code.
- `(copilot-chat-optimize)` ask copilot to optimize selected code.
- `(copilot-chat-test)` ask copilot to write tests for selected code.
- `(copilot-chat-custom-prompt-selection)` ask for a prompt in minibuffer and pastes selection after it before sending it to copilot.
- `(copilot-chat-add-current-buffer)` add current buffer to copilot chat. Its content will be sent with every request.
- `(copilot-chat-list)` open buffer list.
- `(copilot-chat-create)` create a new context. History and buffers are forgotten.
- `copilot-chat-prompt-history-previous` insert previous prompt from history in prompt buffer.
- `copilot-chat-prompt-history-next` insert next prompt from history in prompt buffer.

### Key bindings
#### Prompt buffer
- `C-c RET` send prompt. Answer will be written in chat buffer.
- `M-p` previous prompt.
- `M-n` next prompt.
- `C-c l` open buffer list.
- `C-c q` bury buffer and delete window

#### Chat buffer
- `q` bury buffer

#### Buffer list buffer
- `RET` select or deselect buffer on point
- `space` select or deselect buffer on point
- `C-c` clear buffer list
- `g` refresh list
- `q` bury buffer and delete window

### Shell-maker

`(copilot-chat-shell)` opens a copilot chat shell. History and included buffers work like in copilot prompt buffer but predefined functions (explain, review… will still use chat buffer to display answers).

## TODO
- Manage markdown in buffers
- Use shell for predefined prompts if it is enabled
