# Neovim Docs

This directory now only documents parts of the current native `vim.pack`-based config that are still active or still useful as reference.

## Keep First

- [customization.md](./customization.md)
  Main guide for extending the current config through `lua/user/`
- [nvim-spider.md](./nvim-spider.md)
  Current smart word-motion behavior
- [nvim-spectre.md](./nvim-spectre.md)
  Current project search-and-replace workflow

## Optional Reference

- [python-ty-configuration.md](./python-ty-configuration.md)
  `ty` remains an optional Python LSP / type-checking path
- [snacks-picker-optimization.md](./snacks-picker-optimization.md)
  Historical notes around picker UX; parts of it are still relevant, but some examples may lag behind the current config

## Legacy Notes

Some old plugin notes may still be present in this folder even though the plugins are no longer active in the current setup.

If a doc refers to AstroNvim, Lazy.nvim, preset loading, or plugins not listed in [README.md](/d:/config/dotfiles/nvim/README.md), treat it as historical unless it is rewritten to match the current config.

Typical examples of historical docs:

- `windows-nvim.md`
- `colorful-winsep-nvim.md`
- `harpoon.md`

## Source Of Truth

For the current running config, prefer these files over old docs:

- [README.md](/d:/config/dotfiles/nvim/README.md)
- [customization.md](./customization.md)
- [lua/pack/spec.lua](/d:/config/dotfiles/nvim/lua/pack/spec.lua)
- [lua/plugins/](/d:/config/dotfiles/nvim/lua/plugins)
- [lua/lsp/servers.lua](/d:/config/dotfiles/nvim/lua/lsp/servers.lua)
