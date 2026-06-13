# Neovim Architecture

This document is a short map of the current active configuration. It is meant for maintenance and refactoring, not day-one setup.

## Runtime Order

1. `init.lua`
2. `lua/core`
3. `lua/pack`
4. `lua/plugins`
5. `lua/lsp`
6. `lua/user`

## Ownership Rules

- `core` = editor-wide primitives, commands, keymaps, sessions, workspace helpers
- `pack` = plugin inventory, install/update/repair, lightweight pack UI
- `plugins` = plugin setup and integration wiring
- `lang` = runtime behavior and per-domain logic that may be shared across plugins
- `lsp` = server registry, Mason integration, and LSP activation
- `user` = personal overrides only; prefer extending here before editing the base config, especially through `lua/user/plugins/*.lua`, `lua/user/lsp/*.lua`, and `lua/user/lang.lua`

## Extension Boundaries

- Use `lua/user/init.lua` as the thin user entrypoint that merges optional `theme`, `pack`, `session`, and `lsp_settings` modules.
- Use `lua/user/plugins/*.lua` for extra plugins or plugin-specific additions.
- Use `lua/user/lsp/*.lua` for LSP additions or overrides.
- Use `lua/user/lang.lua` for language runtime preferences such as formatter or parser additions.
- Use `after/ftplugin/*.lua` for filetype-local buffer options and narrow local behavior.

## Current Seams

- `lua/plugins/editing.lua` owns lightweight editing UX like `mini.*` and `nvim-spider`.
- `lua/plugins/completion.lua` owns `blink.cmp` setup only.
- `lua/plugins/git.lua` owns `gitsigns` and hunk keymaps.
- `lua/plugins/syntax.lua` owns Treesitter, Treesitter context, TODO comments, and Trouble wiring.
- `lua/plugins/format.lua` owns Conform, formatter fallbacks, Mason tool installation, and the global format keymap.
- `lua/lang/frontend_state.lua`, `lua/lang/frontend_actions.lua`, and `lua/lang/frontend_filetypes.lua` hold frontend runtime state and helpers so `lua/plugins/frontend.lua` can stay focused on autocmds and keymaps.

## Maintenance Notes

- `README.md` is the active source of truth for supported behavior.
- `docs/customization.md` explains how to extend the config safely.
- `MIGRATION.md`, `LEGACY-FILES.md`, and `ENHANCEMENT_GUIDE.md` are archival context, not runtime authority.
