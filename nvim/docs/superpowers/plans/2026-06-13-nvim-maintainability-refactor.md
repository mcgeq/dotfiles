# Neovim Maintainability Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reduce the maintenance surface of the current Neovim config by making documentation authoritative, consolidating frontend behavior, extracting shared scratch-buffer helpers, and simplifying user/LSP extension boundaries.

**Architecture:** Keep the existing `vim.pack + native lsp + lua/user overrides` direction, but finish the migration by tightening module boundaries. The refactor should move from broad, transition-era files toward smaller domain-focused modules with one clear extension path per concern.

**Tech Stack:** Neovim 0.12+, Lua, `vim.pack`, native `vim.lsp`, PowerShell, `rg`

---

## File Structure

### Existing files to modify

- `nvim/README.md`
  Current top-level source of truth for the active configuration.
- `nvim/docs/README.md`
  Secondary docs index that should stop contradicting top-level guidance.
- `nvim/LEGACY-FILES.md`
  Migration inventory that currently marks still-used docs as removable.
- `nvim/docs/customization.md`
  Main user-facing extension guide; must stay aligned with runtime behavior.
- `nvim/lua/lang/frontend.lua`
  Runtime frontend mode/state loader; should become the single source of frontend config state.
- `nvim/lua/plugins/frontend.lua`
  Frontend plugin wiring, keymaps, save hooks, and frontend-only helpers.
- `nvim/lua/plugins/editor.lua`
  General editing UX plus some frontend formatting ownership logic that should move out.
- `nvim/lua/core/commands.lua`
  Contains scratch-window helpers that are duplicated elsewhere.
- `nvim/lua/core/keymaps.lua`
  Contains scratch-buffer helpers and broad keymap registration.
- `nvim/lua/lsp/init.lua`
  Merges base LSP, user LSP modules, and user inline server overrides.
- `nvim/lua/user/init.lua`
  User override contract; currently exposes derived fields that should become internal.

### New files to create

- `nvim/docs/architecture.md`
  Short current-state architecture map for future maintenance.
- `nvim/lua/lang/frontend/filetypes.lua`
  Shared filetype/root-marker/frontend ownership tables.
- `nvim/lua/lang/frontend/actions.lua`
  Shared frontend code-action and save-action execution helpers.
- `nvim/lua/core/scratch.lua`
  Shared scratch/transient buffer helpers reused by commands and keymaps.

### Validation commands used throughout

- `rg -n "safe to delete|source of truth|historical" nvim/README.md nvim/docs/README.md nvim/LEGACY-FILES.md`
- `rg -n "eslint_filetypes|frontend_owns_format|source.fixAll.eslint|open_scratch_buffer|create_scratch_window|ensure_installed_lookup" nvim/lua`
- `git diff -- nvim`

---

### Task 1: Make Documentation And Migration State Coherent

**Files:**
- Create: `nvim/docs/architecture.md`
- Modify: `nvim/README.md`
- Modify: `nvim/docs/README.md`
- Modify: `nvim/LEGACY-FILES.md`
- Modify: `nvim/docs/customization.md`
- Test: documentation consistency via `rg`

- [ ] **Step 1: Write the failing documentation consistency checklist**

Create a scratch checklist in your task notes with these required outcomes:

```text
1. README names exactly one source-of-truth path for active config behavior.
2. docs/README does not list any file as active if LEGACY-FILES marks it removable.
3. LEGACY-FILES only lists files that are truly archival or unused by README/customization docs.
4. customization.md points users to the current extension entry points without migration-era caveats.
5. architecture.md gives a one-screen overview of runtime flow and ownership boundaries.
```

- [ ] **Step 2: Run the failure check against current files**

Run:

```powershell
rg -n "safe to delete|source of truth|historical|customization.md|docs/README.md|python-ty-configuration.md|nvim-spider.md|nvim-spectre.md" nvim/README.md nvim/docs/README.md nvim/LEGACY-FILES.md nvim/docs/customization.md
```

Expected: evidence that `README.md`, `docs/README.md`, and `LEGACY-FILES.md` describe the same files differently.

- [ ] **Step 3: Update the docs to establish one authoritative contract**

Apply these content changes:

```markdown
# In nvim/README.md
- Keep the current runtime chain section.
- Add a short "Maintenance Contract" section:
  - README.md = active behavior and supported extension model
  - docs/customization.md = how to customize safely
  - docs/architecture.md = how modules are organized today
  - MIGRATION.md / LEGACY-FILES.md / ENHANCEMENT_GUIDE.md = archival only

# In nvim/docs/README.md
- Keep only currently useful docs in "Keep First" and "Optional Reference".
- Add a note that docs not linked from README.md are reference or archive material.
- Remove wording that implies a file is both current and removable.

# In nvim/LEGACY-FILES.md
- Remove any file from "safe to delete" if README.md or docs/README.md still references it.
- Split remaining entries into:
  - "Archive only, keep for reference"
  - "Candidates for deletion after verification"

# In nvim/docs/customization.md
- Add a short "Where to put changes" matrix:
  - lua/user/init.lua = global overrides
  - lua/user/plugins/*.lua = extra plugins
  - lua/user/lsp/*.lua = LSP overrides
  - after/ftplugin/*.lua = filetype-local options
  - domain modules = shared behavior, not personal overrides

# New nvim/docs/architecture.md
# Neovim Architecture

## Runtime Order
1. init.lua
2. lua/core
3. lua/pack
4. lua/plugins
5. lua/lsp
6. lua/user

## Ownership Rules
- core = editor-wide primitives and commands
- pack = plugin inventory, install/update/repair
- plugins = plugin setup and integration wiring
- lang = runtime behavior and per-domain logic
- lsp = server registry and activation
- user = personal overrides only
```

- [ ] **Step 4: Re-run the consistency check**

Run:

```powershell
rg -n "safe to delete|source of truth|historical|Maintenance Contract|Where to put changes|Ownership Rules" nvim/README.md nvim/docs/README.md nvim/LEGACY-FILES.md nvim/docs/customization.md nvim/docs/architecture.md
```

Expected: the wording should now clearly distinguish active docs from archive docs, with no still-active file marked removable.

- [ ] **Step 5: Commit**

```bash
git add nvim/README.md nvim/docs/README.md nvim/LEGACY-FILES.md nvim/docs/customization.md nvim/docs/architecture.md
git commit -m "docs: align nvim maintenance and customization guidance"
```

---

### Task 2: Consolidate Frontend Ownership Tables And Runtime State

**Files:**
- Create: `nvim/lua/lang/frontend/filetypes.lua`
- Modify: `nvim/lua/lang/frontend.lua`
- Modify: `nvim/lua/plugins/editor.lua`
- Modify: `nvim/lua/plugins/frontend.lua`
- Test: `rg`-based structural checks

- [ ] **Step 1: Write the failing structure target**

Use this target as the failure definition:

```text
Frontend filetype ownership should be defined in one module and imported everywhere else.
No module outside lang/frontend* should hardcode the main eslint-owned filetype table.
frontend.lua should remain responsible for state/config, not plugin wiring.
```

- [ ] **Step 2: Verify the current duplication exists**

Run:

```powershell
rg -n "css = true|typescriptreact = true|eslint_filetypes|formatter = \"eslint\"|formatter = \"conform\"" nvim/lua/lang/frontend.lua nvim/lua/plugins/editor.lua nvim/lua/plugins/frontend.lua nvim/lua/user/lang.lua
```

Expected: duplicated or split frontend ownership knowledge across multiple files.

- [ ] **Step 3: Create a shared frontend filetypes module**

Add `nvim/lua/lang/frontend/filetypes.lua` with this shape:

```lua
local M = {}

M.eslint_owned = {
  css = true,
  html = true,
  javascript = true,
  javascriptreact = true,
  json = true,
  jsonc = true,
  less = true,
  scss = true,
  toml = true,
  typescript = true,
  typescriptreact = true,
  vue = true,
  yaml = true,
}

M.visual_filetypes = {
  "css",
  "scss",
  "sass",
  "less",
  "html",
  "javascript",
  "typescript",
  "javascriptreact",
  "typescriptreact",
  "vue",
  "svelte",
}

M.lsp_filetypes = {
  "javascript",
  "javascriptreact",
  "typescript",
  "typescriptreact",
  "vue",
}

function M.build_eslint_lookup(extra_filetypes)
  local lookup = vim.deepcopy(M.eslint_owned)
  for _, filetype in ipairs(extra_filetypes or {}) do
    lookup[filetype] = true
  end
  return lookup
end

return M
```

- [ ] **Step 4: Update frontend state and editor ownership checks to use the shared module**

Make these code-level changes:

```lua
-- In nvim/lua/lang/frontend.lua
local filetypes = require("lang.frontend.filetypes")

-- keep DEFAULTS/PRESETS/state here
-- do not add plugin-specific behavior here

function M.eslint_lookup()
  return filetypes.build_eslint_lookup(M.get().eslint_filetypes)
end

-- In nvim/lua/plugins/editor.lua
local frontend_state = require("lang.frontend")

local function frontend_owns_format(bufnr)
  local frontend = frontend_state.get()
  local eslint_filetypes = frontend_state.eslint_lookup()
  if frontend.formatter == "conform" or not eslint_filetypes[vim.bo[bufnr].filetype] then return false end
  ...
end

-- In nvim/lua/plugins/frontend.lua
local frontend_filetypes = require("lang.frontend.filetypes")

pattern = frontend_filetypes.visual_filetypes
pattern = frontend_filetypes.lsp_filetypes
```

- [ ] **Step 5: Validate there is now a single frontend ownership table**

Run:

```powershell
rg -n "css = true|typescriptreact = true|eslint_owned|build_eslint_lookup|visual_filetypes|lsp_filetypes" nvim/lua/lang/frontend.lua nvim/lua/lang/frontend/filetypes.lua nvim/lua/plugins/editor.lua nvim/lua/plugins/frontend.lua
```

Expected: the hardcoded lookup table should live only in `lang/frontend/filetypes.lua`, with the other modules consuming it.

- [ ] **Step 6: Commit**

```bash
git add nvim/lua/lang/frontend.lua nvim/lua/lang/frontend/filetypes.lua nvim/lua/plugins/editor.lua nvim/lua/plugins/frontend.lua
git commit -m "refactor: centralize frontend filetype ownership"
```

---

### Task 3: Extract Shared Frontend Actions And Shrink Plugin Wiring

**Files:**
- Create: `nvim/lua/lang/frontend/actions.lua`
- Modify: `nvim/lua/plugins/frontend.lua`
- Test: `rg`-based structural checks and manual action map review

- [ ] **Step 1: Write the failing module-boundary target**

Use this target:

```text
plugins/frontend.lua should register autocmds and keymaps, but not own the low-level implementation of:
- has_client
- apply_sync_code_action
- preferred code action dispatch
- frontend save-action execution
```

- [ ] **Step 2: Confirm the current boundary is too broad**

Run:

```powershell
rg -n "has_client|apply_sync_code_action|preferred_code_action|source.organizeImports|source.removeUnused.ts|source.addMissingImports.ts" nvim/lua/plugins/frontend.lua
```

Expected: the plugin module currently owns all action logic directly.

- [ ] **Step 3: Create a shared frontend actions helper**

Add `nvim/lua/lang/frontend/actions.lua` with this shape:

```lua
local M = {}

local frontend_clients = { "eslint", "vtsls", "vue_ls" }

function M.has_client(bufnr, names)
  local lookup = {}
  for _, name in ipairs(names or {}) do
    lookup[name] = true
  end
  for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
    if lookup[client.name] then return true end
  end
  return false
end

function M.show_frontend_clients(bufnr)
  local names = {}
  for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
    if vim.list_contains(frontend_clients, client.name) then
      table.insert(names, client.name)
    end
  end
  table.sort(names)
  vim.notify("Frontend LSP: " .. (#names > 0 and table.concat(names, ", ") or "none"), vim.log.levels.INFO, {
    title = "Frontend Clients",
  })
end

function M.apply_sync_code_action(bufnr, only, client_names)
  -- move current implementation here unchanged first
end

function M.preferred_code_action(only, client_names)
  return function()
    local bufnr = vim.api.nvim_get_current_buf()
    if only == "source.fixAll.eslint" and vim.fn.exists(":EslintFixAll") == 2 and M.has_client(bufnr, { "eslint" }) then
      pcall(vim.cmd, "silent EslintFixAll")
      return
    end
    if M.apply_sync_code_action(bufnr, only, client_names) then return end
    vim.lsp.buf.code_action({
      apply = true,
      context = {
        only = { only },
        diagnostics = vim.diagnostic.get(bufnr),
      },
    })
  end
end

function M.run_save_actions(bufnr, frontend)
  local save_actions = frontend.save_actions or {}
  if frontend.eslint_fix_on_save == false then return end
  if save_actions.eslint and M.has_client(bufnr, { "eslint" }) then
    if vim.fn.exists(":EslintFixAll") == 2 then
      pcall(vim.cmd, "silent EslintFixAll")
    else
      M.apply_sync_code_action(bufnr, "source.fixAll.eslint", { "eslint" })
    end
  end
  if save_actions.organize_imports and M.has_client(bufnr, { "vtsls" }) then
    M.apply_sync_code_action(bufnr, "source.organizeImports", { "vtsls" })
  end
  if save_actions.remove_unused and M.has_client(bufnr, { "vtsls" }) then
    M.apply_sync_code_action(bufnr, "source.removeUnused.ts", { "vtsls" })
  end
  if save_actions.add_missing_imports and M.has_client(bufnr, { "vtsls" }) then
    M.apply_sync_code_action(bufnr, "source.addMissingImports.ts", { "vtsls" })
  end
end

return M
```

- [ ] **Step 4: Reduce plugins/frontend.lua to registration and lazy plugin setup**

Update `nvim/lua/plugins/frontend.lua` toward this shape:

```lua
local actions = require("lang.frontend.actions")
local frontend_filetypes = require("lang.frontend.filetypes")

-- keep ensure_colorizer / ensure_autotag here
-- replace inline action logic with actions.preferred_code_action(...)
-- replace show_frontend_clients with actions.show_frontend_clients(...)
-- replace save hook body with actions.run_save_actions(event.buf, frontend_state.get())
```

- [ ] **Step 5: Validate that action logic moved out**

Run:

```powershell
rg -n "apply_sync_code_action|preferred_code_action|run_save_actions|show_frontend_clients" nvim/lua/lang/frontend/actions.lua nvim/lua/plugins/frontend.lua
```

Expected: helper implementations live in `lang/frontend/actions.lua`, while `plugins/frontend.lua` mainly wires autocmds and keymaps.

- [ ] **Step 6: Commit**

```bash
git add nvim/lua/lang/frontend/actions.lua nvim/lua/plugins/frontend.lua
git commit -m "refactor: extract frontend action helpers"
```

---

### Task 4: Extract Shared Scratch/Transient Buffer Helpers

**Files:**
- Create: `nvim/lua/core/scratch.lua`
- Modify: `nvim/lua/core/commands.lua`
- Modify: `nvim/lua/core/keymaps.lua`
- Test: `rg`-based structural checks

- [ ] **Step 1: Define the failing duplication target**

Use this target:

```text
Scratch/transient buffer creation should have one reusable implementation.
Neither core/commands.lua nor core/keymaps.lua should directly repeat the same nofile/wipe/swapfile/q-close setup block.
```

- [ ] **Step 2: Confirm the duplication exists**

Run:

```powershell
rg -n "buftype = \"nofile\"|bufhidden = \"wipe\"|swapfile = false|buflisted = false|buf_set_name\\(bufnr, \"scratch\"" nvim/lua/core/commands.lua nvim/lua/core/keymaps.lua
```

Expected: matching scratch/transient setup patterns in both files.

- [ ] **Step 3: Create the shared scratch helper**

Add `nvim/lua/core/scratch.lua` with this shape:

```lua
local M = {}

function M.create(name, opts)
  opts = opts or {}
  vim.cmd(opts.command or "botright new")
  local bufnr = vim.api.nvim_get_current_buf()
  vim.bo[bufnr].buftype = "nofile"
  vim.bo[bufnr].bufhidden = "wipe"
  vim.bo[bufnr].swapfile = false
  vim.bo[bufnr].buflisted = false
  vim.bo[bufnr].modifiable = opts.modifiable ~= false
  vim.bo[bufnr].filetype = opts.filetype or "text"
  vim.api.nvim_buf_set_name(bufnr, name)
  vim.keymap.set("n", "q", "<cmd>close<cr>", {
    buffer = bufnr,
    silent = true,
    desc = opts.close_desc or "Close window",
  })
  return bufnr
end

function M.set_lines(bufnr, lines)
  if not vim.api.nvim_buf_is_valid(bufnr) then return end
  vim.bo[bufnr].modifiable = true
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)
  vim.bo[bufnr].modifiable = false
end

function M.open(name, lines, opts)
  local bufnr = M.create(name, opts)
  if lines then
    M.set_lines(bufnr, lines)
    vim.api.nvim_win_set_cursor(0, { 1, 0 })
  end
  return bufnr
end

return M
```

- [ ] **Step 4: Replace duplicated helpers with the shared module**

Update the two callers like this:

```lua
-- In nvim/lua/core/commands.lua
local scratch = require("core.scratch")

local function open_scratch_window(name, lines, filetype)
  return scratch.open(name, lines, { filetype = filetype })
end

-- In nvim/lua/core/keymaps.lua
local scratch = require("core.scratch")

local function open_scratch_buffer()
  scratch.open("scratch", nil, {
    filetype = "text",
    close_desc = "Close scratch buffer",
  })
end
```

- [ ] **Step 5: Validate the duplication is gone**

Run:

```powershell
rg -n "create_scratch_window|set_scratch_lines|open_scratch_window|buftype = \"nofile\"|bufhidden = \"wipe\"" nvim/lua/core/commands.lua nvim/lua/core/keymaps.lua nvim/lua/core/scratch.lua
```

Expected: the reusable setup should live in `core/scratch.lua`, with the two callers reduced to small wrappers.

- [ ] **Step 6: Commit**

```bash
git add nvim/lua/core/scratch.lua nvim/lua/core/commands.lua nvim/lua/core/keymaps.lua
git commit -m "refactor: share scratch buffer helpers"
```

---

### Task 5: Simplify The User/LSP Override Contract

**Files:**
- Modify: `nvim/lua/lsp/init.lua`
- Modify: `nvim/lua/user/init.lua`
- Modify: `nvim/docs/customization.md`
- Test: `rg`-based structural checks

- [ ] **Step 1: Define the failing contract target**

Use this target:

```text
User config should expose declarative inputs only.
Derived fields such as ensure_installed_lookup should be internal implementation details.
There should be one recommended way to add an LSP override: lua/user/lsp/*.lua.
```

- [ ] **Step 2: Confirm the current derived-field leak**

Run:

```powershell
rg -n "ensure_installed_lookup|ensure_installed = \\{\\}|servers = \\{\\}|lua/user/lsp" nvim/lua/user/init.lua nvim/lua/lsp/init.lua nvim/docs/customization.md
```

Expected: `ensure_installed_lookup` is exposed in user config and consumed by `lsp/init.lua`.

- [ ] **Step 3: Move the derived lookup into lsp/init.lua**

Change the code toward this shape:

```lua
-- In nvim/lua/user/init.lua
lsp = {
  ensure_installed = {},
  auto_install = true,
}

-- In nvim/lua/lsp/init.lua
local function to_lookup(names)
  local lookup = {}
  for _, name in ipairs(names or {}) do
    lookup[name] = true
  end
  return lookup
end

local function build_server_registry()
  local registry = require("lsp.servers").get()
  local user = require("user")
  local ensure_lookup = to_lookup(user.lsp.ensure_installed)

  for _, entry in ipairs(util.load_table_modules("user.lsp")) do
    if entry.name and entry.config then
      entry.ensure_installed = entry.ensure_installed or ensure_lookup[entry.name]
      registry[entry.name] = entry
    end
  end

  return registry
end
```

If you keep `user.lsp.servers` temporarily for compatibility, mark it as legacy in docs and avoid recommending it going forward.

- [ ] **Step 4: Update customization guidance to recommend one path**

Add or update doc wording like this:

```markdown
## Preferred LSP Extension Path

- Add new servers or overrides in `lua/user/lsp/*.lua`
- Use `lsp.ensure_installed = { ... }` only to request Mason-backed installs
- Avoid putting full server configs in `lua/user/init.lua` unless you are preserving backward compatibility during migration
```

- [ ] **Step 5: Validate the contract is simpler**

Run:

```powershell
rg -n "ensure_installed_lookup|Preferred LSP Extension Path|user.lsp.servers" nvim/lua/user/init.lua nvim/lua/lsp/init.lua nvim/docs/customization.md
```

Expected: `ensure_installed_lookup` should be gone from `user/init.lua`, and docs should recommend the file-based override path.

- [ ] **Step 6: Commit**

```bash
git add nvim/lua/lsp/init.lua nvim/lua/user/init.lua nvim/docs/customization.md
git commit -m "refactor: simplify nvim user lsp overrides"
```

---

## Self-Review

### Spec coverage

- Documentation conflict reduction: covered by Task 1.
- Frontend consolidation: covered by Tasks 2 and 3.
- Shared scratch/transient buffer helpers: covered by Task 4.
- User/LSP contract simplification: covered by Task 5.

### Placeholder scan

- No `TODO`, `TBD`, or “implement later” markers remain in the task steps.
- Every task includes concrete files, commands, and target code shapes.

### Type consistency

- `lang/frontend/filetypes.lua` is the shared ownership table module in both Task 2 and Task 3.
- `lang/frontend/actions.lua` is the shared action implementation module in Task 3.
- `core/scratch.lua` is the shared transient buffer helper in Task 4.
- `lsp.ensure_installed` remains the public list input in Task 5.

