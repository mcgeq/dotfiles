# Legacy Files To Remove

These files are no longer part of the active native Neovim startup chain.

## Already removed from the working tree

- `lua/community.lua`
- `lua/lazy_setup.lua`
- `lua/polish.lua`
- `lua/config/*`
- `lua/plugins/astrocore.lua`
- `lua/plugins/astrolsp.lua`
- `lua/plugins/astroui.lua`
- `lua/plugins/backend-enhanced.lua`
- `lua/plugins/frontend-enhanced.lua`
- `lua/plugins/jujutsu.lua`
- `lua/plugins/mason.lua`
- `lua/plugins/none-ls.lua`
- `lua/plugins/nvim-spectre.lua`
- `lua/plugins/python-ty-ruff.lua`
- `lua/plugins/python-venv-auto.lua`
- `lua/plugins/user.lua`

## Archive only, keep for reference

The following files are not part of the active startup chain, but some are still worth keeping as migration or maintenance reference:

- `lua/plugins/conform.lua`
- `lua/plugins/flash.nvim.lua`
- `lua/plugins/noice-config.lua`
- `lua/plugins/snacks.nvim.lua`
- `lua/plugins/treesitter.lua`
- `lua/plugins/ui-enhanced.lua`
- `docs/README.md`
- `docs/nvim-spectre.md`
- `docs/nvim-spider.md`
- `docs/python-ty-configuration.md`
- `docs/snacks-picker-optimization.md`
- `ENHANCEMENT_GUIDE.md`

## Candidates for deletion after verification

These files look archival and are not part of the active config. Verify nobody still relies on them before removal:

- `docs/colorful-winsep-nvim.md`
- `docs/edgy-nvim.md`
- `docs/harpoon.md`
- `docs/mini-ai.md`
- `docs/neorg-guide.md`
- `docs/neorg-quickref.md`
- `docs/neorg.md`
- `docs/neotest.md`
- `docs/nvim-toggler.md`
- `docs/project-nvim.md`
- `docs/rainbow-delimiter-indent-blankline.md`
- `docs/toggleterm-nvim.md`
- `docs/windows-nvim.md`
- `lazy-lock.json`
- `.preset`
- `nvim.log`
- `neovim.yml`

Use `CLEANUP-LEGACY.ps1` to remove the remaining files.
