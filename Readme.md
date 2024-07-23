# Copilot Chat for Emacs
## Description
This plugin allows you to chat with github copilot.

![copilot-chat demo](chat.gif?raw=true "copilot-chat demo")

This plugin is unofficial and based on Copilot Chat for neovim repository: https://github.com/CopilotC-Nvim/CopilotChat.nvim

Feel free to contribute (check the [Todo list](#todo) ), report issues or discuss new features.

## Installation
### Straight
```
(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el")))
```

### Manual
Clone repository and eval files in emacs.

## Configuration
### Github
You will need a github acccount with access to copilot API. When sending the first prompt, you will need to authenticate to github. Follow instructions and everything will be fine.

### Network 
If `copilot-chat-use-curl` is set to t (default), copilot-chat will use curl program for https requests. The path can be set in `copilot-chat-curl-program`. You can switch back to emacs url library by setting `copilot-chat-use-curl` to nil.

With curl, answers will be written token by token.  
With url library, all the text is written when all data is read and this can be long.

### Frontend
Several frontends are available. You can select your favorite by setting the `copilot-chat-frontend` variable to `'markdown` or `'org`.

You can also use `'shell-maker` if you want ( https://github.com/xenodium/chatgpt-shell ).
```
(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :custom
  (copilot-chat-frontend 'shell-maker)
  :config
  (require 'copilot-chat-shell-maker)
  (push '(shell-maker . copilot-chat-shell-maker-init) copilot-chat-frontend-list))
```

Try them and choose wisely.

## Usage
### Functions
- `(copilot-chat-reset)` reset everything including history, buffers and frontend.
- `(copilot-chat-display)` display copilot chat buffers.
- `(copilot-chat-explain)` ask copilot to explain selected code.
- `(copilot-chat-review)` ask copilot to review selected code.
- `(copilot-chat-doc)` ask copilot to document selected code.
- `(copilot-chat-fix)` ask copilot to fix selected code.
- `(copilot-chat-optimize)` ask copilot to optimize selected code.
- `(copilot-chat-test)` ask copilot to write tests for selected code.
- `(copilot-chat-custom-prompt-selection)` ask for a prompt in minibuffer and pastes selection after it before sending it to copilot.
- `(copilot-chat-add-current-buffer)` add current buffer to copilot chat. Its content will be sent with every request.
- `(copilot-chat-list)` open buffer list.
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
