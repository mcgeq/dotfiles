# Neovim Docs

This directory documents the current native `vim.pack`-based config and keeps a small amount of reference material that is still useful during maintenance.

## Keep First

- [customization.md](./customization.md)
  Main guide for extending the current config through `lua/user/`
- [architecture.md](./architecture.md)
  Current runtime order and module ownership map
- [nvim-spider.md](./nvim-spider.md)
  Current smart word-motion behavior
- [nvim-spectre.md](./nvim-spectre.md)
  Current project search-and-replace workflow

## Optional Reference

- [cpp-clangd.md](./cpp-clangd.md)
  C++20/23, clangd, query-driver, and module workflow notes
- [python-ty-configuration.md](./python-ty-configuration.md)
  `ty` remains an optional Python LSP / type-checking path
- [snacks-picker-optimization.md](./snacks-picker-optimization.md)
  Historical notes around picker UX; parts of it are still relevant, but some examples may lag behind the current config

## Reference Notes

Some older notes may still be present in this folder even though the plugins or workflows they describe are no longer active.

If a document refers to AstroNvim, Lazy.nvim, preset loading, or plugins not listed in [README.md](/d:/config/dotfiles/nvim/README.md), treat it as reference or archive material unless it has been rewritten to match the current config.

Typical archive-style examples:

- `windows-nvim.md`
- `colorful-winsep-nvim.md`
- `harpoon.md`

## Source Of Truth

For the current running config, prefer these files over old docs:

- [README.md](/d:/config/dotfiles/nvim/README.md)
- [customization.md](./customization.md)
- [architecture.md](./architecture.md)
- [lua/pack/spec.lua](/d:/config/dotfiles/nvim/lua/pack/spec.lua)
- [lua/plugins/](/d:/config/dotfiles/nvim/lua/plugins)
- [lua/lsp/servers.lua](/d:/config/dotfiles/nvim/lua/lsp/servers.lua)

If a note is not linked from `README.md` or this index, do not treat it as active guidance without re-verifying it against the current code.
