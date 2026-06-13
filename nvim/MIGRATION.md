# Migration Status

The configuration is being rebuilt around these goals:

- native `vim.pack`
- native Neovim LSP APIs
- stable base config
- user extensions isolated under `lua/user/`
- filetype behavior moved toward `after/ftplugin/`

## Active runtime

The active runtime path now starts from:

- [`init.lua`](/d:/config/dotfiles/nvim/init.lua)
- [`lua/core/`](/d:/config/dotfiles/nvim/lua/core)
- [`lua/pack/`](/d:/config/dotfiles/nvim/lua/pack)
- [`lua/plugins/`](/d:/config/dotfiles/nvim/lua/plugins)
- [`lua/lsp/`](/d:/config/dotfiles/nvim/lua/lsp)
- [`lua/user/`](/d:/config/dotfiles/nvim/lua/user)

## Reference-only legacy areas

These paths are still present for comparison and staged migration, but are no longer part of the new startup chain:

- `lua/lazy_setup.lua`
- `lua/community.lua`
- `lua/polish.lua`
- `lua/config/*`
- legacy Astro-oriented files under `lua/plugins/*`

## Next cleanup targets

- migrate useful language-specific modules into the new structure
- replace legacy docs with native-config docs
- remove AstroNvim-specific files after feature parity is good enough
