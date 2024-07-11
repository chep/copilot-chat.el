# Copilot Chat for Emacs
## Notes
This plugin is based on Copilot Chat for neovim repository: https://github.com/CopilotC-Nvim/CopilotChat.nvim

This is a work in progress. Feel free to help me. See in the [Todo list](#todo) if you can implement missing features.

## Installation
### Straight
TODO

### Manual
Clone repository and eval files in emacs.

## Configuration
You will need a token. The token can be put in the function `copilot-chat-get-cached-token` in `copilot-chat-copilot.el`

There is no way (for the moment) to get the token in emacs. I got mine with neovim: install the copilot-chat plugin (see in [Notes](#notes)), connect and the token should be in `~/.config/github-copilot/hosts.json`.

## Usage
`(copilot-display)` displays copilot chat and prompt buffer.

While in prompt buffer, enter a prompt and hit `C-c RET` to send it. The answer will be written in chat buffer.

`(copilot-chat-explain)` asks copilot to explain selected code.

`(copilot-chat-review)` asks copilot to review selected code.

`(copilot-chat-doc)` asks copilot to document selected code.

`(copilot-chat-fix)` asks copilot to fix selected code.

`(copilot-chat-optimize)` asks copilot to optimize selected code.

`(copilot-chat-test)` asks copilot to write tests for selected code.

`(copilot-chat-custom-prompt-selection) asks for a prompt in minibuffer and pastes selection after it before sending it to copilot.

`(copilot-chat-add-current-buffer)` adds current buffer to copilot chat. Its content will be sent with every request.

`(copilot-chat-create)` creates a new context. History and buffers are forgotten.

## TODO
- Get github token
- Prompt history

