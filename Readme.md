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

### Manual
Clone repository and eval files in emacs.

## Configuration
When sending the first prompt, you will need to authenticate to github. Follow instructions and everything will be fine.

## Usage
`(copilot-display)` displays copilot chat and prompt buffer.

While in prompt buffer, enter a prompt and hit `C-c RET` to send it. The answer will be written in chat buffer.

`(copilot-chat-explain)` asks copilot to explain selected code.

`(copilot-chat-review)` asks copilot to review selected code.

`(copilot-chat-doc)` asks copilot to document selected code.

`(copilot-chat-fix)` asks copilot to fix selected code.

`(copilot-chat-optimize)` asks copilot to optimize selected code.

`(copilot-chat-test)` asks copilot to write tests for selected code.

`(copilot-chat-custom-prompt-selection)` asks for a prompt in minibuffer and pastes selection after it before sending it to copilot.

`(copilot-chat-add-current-buffer)` adds current buffer to copilot chat. Its content will be sent with every request.

`(copilot-chat-list)` open buffer list. Buffers can be added or removed to copilot context with space and RET. `C-c c` clears the list. This function can be called with `C-c l` in prompt buffer.

`(copilot-chat-create)` creates a new context. History and buffers are forgotten.


## TODO
- Prompt history
- Manage markdown in buffers
