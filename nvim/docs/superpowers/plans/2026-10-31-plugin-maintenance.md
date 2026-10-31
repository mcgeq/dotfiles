# Plugin Maintenance Implementation Plan

**Goal:** Apply approved option 1: replace Spectre with grug-far, remove the
Mason-DAP bridge and mini.comment setup, and remove obsolete Headlines records.

**Architecture:** Keep plugin ownership in `pack.spec`, lazy search loading in
`plugins.search`, and adapter ownership in `plugins.debug`. Use absolute search
paths and a named instance/history directory per workspace. Keep existing keys.

**Tech Stack:** Lua, native vim.pack, grug-far/ripgrep, existing headless checks.

- [ ] Add regression coverage to `tests/behavior.lua` for lazy search mappings,
  project/file scopes, history restoration, and old feature-switch compatibility.
  Run `nvim --headless -u NONE -i NONE -l nvim/tests/behavior.lua` before implementation.
- [ ] Replace the search wrapper and catalog entry; migrate `spectre` feature to
  `search_replace`, honoring the old switch unless the new switch is explicit.
- [ ] Remove the bridge in `plugins/debug.lua` and mini.comment setup in
  `plugins/editing.lua`; retain adapters, Mason tools, and remaining mini modules.
- [ ] Use native vim.pack to add grug-far and delete only nvim-spectre,
  mason-nvim-dap.nvim, and headlines.nvim; preserve every other locked revision.
- [ ] Update both READMEs and the search guide. Exercise real grug-far search,
  scoped replacement, quickfix, hide/resume, and native commenting in disposable
  fixtures through `tests/startup.lua`.
- [ ] Run `scripts/check.ps1 -SkipStartupTiming`, `scripts/offline-startup.ps1`,
  and `git diff --check`; review the final diff. Real-project acceptance and
  Neovim 0.12 compatibility testing remain deferred by user choice.
