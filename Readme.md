# Copilot Chat for Emacs
## Notes
This plugin is based on Copilot Chat for neovim repository: https://github.com/CopilotC-Nvim/CopilotChat.nvim

This is a work in progress. Feel free to help me. See in the [Todo list](#TODO) if you can implement missing features.

## Installation
### Straight
TODO

### Manual
Clone repository and eval files in emacs.

## Usage
`(copilot-display) should display copilot chat and prompt buffer

While in prompt buffer, enter a prompt and hit `C-c RET` to send it. The answer will be written in chat buffer.

`(copilot-chat-explain)` asks copilot to explain selected code

`(copilot-chat-review)` asks copilot to review selected code

`(copilot-chat-doc)` asks copilot to document selected code

`(copilot-chat-fix)` asks copilot to fix selected code

`(copilot-chat-optimize)` asks copilot to optimize selected code

`(copilot-chat-test)` asks copilot to write tests for selected code


## TODO
- Fix display function
- Manage non ASCII char in prompt
- Autoload
- Installation
