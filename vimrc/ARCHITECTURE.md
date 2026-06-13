# Vimrc Architecture

This repository intentionally keeps both `modules/` and `config/`.

`config/` is not a deprecated directory. It is the shared configuration layer.
The goal is to keep ownership clear, reduce keymap conflicts, and avoid adding
custom loading systems where Vim already provides a native mechanism.

## Ownership

- `bootstrap/`
  Environment detection, constants, and baseline editor settings.
  This layer should stay simple and should not contain plugin setup.

- `core/`
  Shared runtime helpers such as the loader, error handling, utility
  functions, module helper primitives, and health/reporting support.

- `modules/`
  Feature modules with real setup logic.
  Use this layer for plugin integration, helper functions, state, and health
  checks.

- `config/`
  Shared configuration with little or no runtime logic.
  This is the right place for:
  - `config/mapping/*.vim` as the centralized owner of global keymaps
    registered through `core/keymap.vim`
  - plugin-native static config files such as `config/coc-settings.json`
  - small declarative tables shared across modules

  This is not the right place for:
  - plugin bootstrap code
  - feature modules with stateful logic
  - language loaders like the old `config/lang/*`

- `after/ftplugin/`
  Filetype-local behavior.
  Put indentation, `makeprg`, compiler settings, and buffer-local mappings here.
  Prefer this over custom language autoload logic.
  You can start from `after/ftplugin/ftplugin.template.vim` when adding a new
  filetype-local override.

- `local/`
  User-specific and machine-local overrides that should not redefine shared
  module ownership.
  Prefer `local/module_overrides.vim` when you want to override `modules/*`
  config fields without editing the shared module files.

## Decision Guide

When adding new behavior:

1. If it initializes a plugin or owns feature logic, put it in `modules/`.
2. If it is a shared global keymap or static plugin config, put it in `config/`.
3. If it only applies to one filetype or buffer, put it in `after/ftplugin/`.
4. If it is personal or machine-specific, put it in `local/`.

## Rules

- Do not reintroduce `config/lang/*`.
- Do not scatter global mappings back into `modules/*`.
- Keep `config/` small and declarative.
- Keep `init.vim` declarative too. Prefer explicit file lists and small loader
  helpers over repeated `if isdirectory(...)` / `source` blocks.
- When a feature module can report useful state, expose a `g:*HealthCheck()`
  function so `core/health.vim` can include it in the shared report.
- Prefer Vim's native runtime mechanisms when possible.

## Module Convention

Every file in `modules/*` should follow the same top-level shape:

```vim
vim9script

if g:MarkModuleLoaded('module_id')
  finish
endif

var config = {
  enabled: true,
}

def g:InitModule(user_config: dict<any> = {})
  config = g:ResolveModuleConfig('module_id', config, user_config)
  if g:ModuleIsDisabled(config, 'Module')
    return
  endif
enddef

def g:ModuleHealthCheck(): dict<any>
  return g:BuildManagedModuleHealth('module_id', 'Module', config)
enddef

call g:InitModule()
```

You can start from `modules/module.template.vim` when adding a new shared
feature module.

- Use `g:MarkModuleLoaded()` as the only duplicate-load guard in shared modules.
- Do not hand-roll `g:mcge_*_loaded` checks inside individual module files.
- Keep the guard at the top of the file so re-sourcing exits before any side effect.
- Keep file headers consistent.
  Use `模块:`, `作者:`, and `说明:` for files under `modules/`.
  Use `组件:`, `作者:`, and `说明:` for shared files under `core/`,
  `bootstrap/`, and `local/`.
- Keep the public module surface small. Prefer exporting only
  `g:Init*()`, `g:Get*Config()`, `g:*HealthCheck()`, and explicit
  user-facing command helpers that must be callable from outside the script.
- Files under `core/`, `bootstrap/`, and `local/` are sourced directly.
  Prefer plain `def g:*` there instead of `export def`, unless the file is
  intentionally used as a Vim9 `import` target.
- Keep internal helpers script-local whenever they do not need to be called
  externally.
- Prefer verb-first names for script-local helpers so their role is obvious:
  `Apply*`, `Register*`, `Configure*`, `Read*`, `Print*`, `Is*`.
- Avoid vague local helper prefixes like `Setup*` when a more specific action
  name is available.
- When a Vim9 module defines persistent `autocmd` or `:command` entries,
  prefer calling stable `g:*` wrappers instead of binding script-local
  functions or direct external calls that can turn into stale `<SNR>` refs
  after re-sourcing.
- Reuse `g:ResolveModuleConfig()` / `g:ResolveModuleConfigDeep()`,
  `g:ModuleIsDisabled()`, and the managed health builders before introducing
  new per-module boilerplate.
- Use booleans for module-level switches such as `enabled` unless the target
  plugin explicitly requires a numeric flag.
- When a module mainly forwards config values into plugin globals, prefer
  `g:ApplyGlobalVars()` over repeated `g:plugin_var = config.some_value` lines.
- When a module config contains nested tables such as `signs`, `layout`, or
  `filetypes`, prefer `g:ResolveModuleConfigDeep()` so local overrides can
  change one nested field without replacing the whole subtree.
- When a module health check mainly tests whether a user command exists, prefer
  `g:BuildManagedCommandModuleHealth()` over open-coded
  `available: exists(':Cmd')`.
- When a module health check mainly depends on an autoload or global function,
  prefer `g:BuildManagedFunctionModuleHealth()` over open-coded
  `exists('*Func')`.
- Use `local/module_overrides.vim` as the default user entrypoint for shared
  module config changes. Keep `user_settings.vim` for editor-local commands,
  autocmd, and final personal tweaks.

## Declarative Keymaps

`config/mapping/*.vim` should prefer declarative spec tables over ad-hoc
mapping commands.

- Use `g:MapMany()`, `g:CmdMapMany()`, and `g:PlugMapMany()` to register
  mappings from data tables.
- Put user-facing labels in the same spec with `desc` so which-key metadata is
  generated from the mapping source instead of being duplicated elsewhere.
- Use `g:WhichKeyGroupMany()` in the same mapping file to declare stable group
  names for prefixes such as `g`, `o`, or `v`.
- Reserve bare leader prefixes for groups once they become namespaces. Put the
  direct action on an explicit second key such as `ee`, `vv`, `aa`, or `af`.
- Keep `modules/ui/whichkey.vim` presentation-only. It should read the
  generated registry, not carry a second copy of key labels.
- Keep keymap docs generated from the mapping registry. Use `doc_section` on
  shared mappings when you want them grouped cleanly in `docs/keymaps.md`.
- `g:CmdMap()` already normalizes many common Ex/plugin command names for the
  generated action column. Use `doc_action` only when you need wording that
  should explicitly override the default command naming.
- Use `doc_action` on complex expr mappings when the real rhs is too noisy for
  the generated cheat sheet.
- Use `doc_desc` when the generated cheat sheet needs a clearer phrase than the
  short label shown in which-key. Use `whichkey_desc` only when the menu label
  itself should diverge from the default `desc`.
- At the top of each mapping file, prefer a few `const ..._defaults = {...}`
  tables for repeated `mode`, `silent`, `nowait`, `clear_cmdline`, and
  `doc_section` values instead of repeating the same options on every
  `g:*MapMany()` call.
- If a mapping file has one or two standalone `g:Map()` / `g:CmdMap()` /
  `g:PlugMap()` calls that should follow the same defaults as the bulk tables,
  prefer `g:MapSpec(spec, defaults)` rather than open-coding the repeated
  fields again.
- Keep the mapping files mostly "data first":
  declare defaults, declare which-key groups, declare spec lists, then register
  them with one `g:*MapMany()` call per list.
- After changing shared mappings or registry helpers, run
  `pwsh -File vimrc/scripts/verify_keymaps.ps1` to regenerate docs and verify
  encoding plus formatting checks.

## Migration Direction

- Keep `config/`, but keep it narrow.
- Keep plugin setup in `modules/`.
- Keep global keymap ownership in `config/mapping/`.
- Keep language-local behavior in `after/ftplugin/`.

This split is about maintainable ownership boundaries, not about satisfying a
special Vim9script requirement.
