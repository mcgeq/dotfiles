# Neovim Runtime Maintenance Implementation Plan

**Goal:** Implement the approved maintenance recommendations in order, retaining every plugin.

**Architecture:** Keep native vim.pack and LSP. Use one formatting policy, one tools installer, declarative plugin lifecycle metadata, and shared language runners. Preserve existing commands and local keymaps through compatibility aliases.

**Tech Stack:** Neovim 0.12+, Lua, PowerShell, Selene, StyLua.

## 1. Runtime correctness

- [x] Add headless behavior regressions for repeated package buffers, save toggles, unavailable frontend tools, project Python isolation, and lockfile restore.
- [x] Fix package-info caching; resolve format providers by filetype and available clients.
- [x] Use the same provider for manual and save formatting, with independent fix/import switches.
- [x] Resolve Python environments without changing process PATH or VIRTUAL_ENV; use per-client/per-run configuration.
- [x] Correct PackSync semantics and preserve it as a compatibility alias.

## 2. Maintenance entrypoints

- [x] Consolidate Mason setup and installation ownership, keeping all integration plugins.
- [x] Centralize plugin lifecycle metadata and enforce feature/disable choices in loading and setup.
- [x] Share plugin health inspection and report user module execution errors.
- [x] Share code-action execution, remove Biome's process-wide binary cache, and update deprecated diagnostic navigation.

## 3. Language workflow integration

- [x] Move C++ execution, detection, and UI out of user overrides; use the shared runner executor.
- [x] Add C/C++ runner dispatch and retain C++/CMake keymaps.
- [x] Document extension contracts, installation, save behavior, and compatibility decisions.

## Verification

For each behavior, run its regression before and after the implementation:

```powershell
nvim --headless -u NONE -i NONE -l nvim/tests/behavior.lua
nvim --headless -u NONE -i NONE -l nvim/tests/smoke.lua
nvim --headless -u NONE -i NONE -l nvim/tests/verify_servers.lua
pwsh -NoProfile -File nvim/scripts/check.ps1 -SkipStartupTiming
pwsh -NoProfile -File nvim/scripts/offline-startup.ps1 -PluginRoot <installed-data>/site/pack/core/opt
git diff --check
```

Use isolated temporary config/data directories for an offline real-plugin startup check. Do not update plugins, change lock revisions, or run network installers during verification.

## Verification results

- Selene: 0 errors, 0 warnings, 0 parse errors.
- StyLua: all Lua/test files formatted.
- Module loading: 85 modules; LSP registry: 18 servers.
- Behavior regressions: 20 passed, including project-local tools, disabled extensions and unsupported C++ actions.
- Offline integration: real installed plugin setup with Lua, C++, CMake, Markdown, TypeScript, Python and SQL buffers.
- Inventory: 48 base plugin sources and 2 C++ extension sources unchanged; lockfile unchanged.
- Independent review findings addressed: unnamed disabled extensions, project-local Ruff/Ty selection, stale CMake repeat context.

Real LSP processes, debug sessions, actual compiler runs, startup timing and Neovim 0.12 stable were not exercised. The installed runtime is Neovim 0.13 development. All changes remain uncommitted for review.
