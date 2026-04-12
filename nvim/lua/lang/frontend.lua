local M = {}

local DEFAULTS = {
  formatter = "eslint",
  eslint_fix_on_save = true,
  save_actions = {
    eslint = true,
    organize_imports = false,
    remove_unused = false,
    add_missing_imports = false,
  },
  eslint_filetypes = {},
}

local PRESETS = {
  eslint = {
    formatter = "eslint",
    eslint_fix_on_save = true,
    save_actions = {
      eslint = true,
      organize_imports = false,
      remove_unused = false,
      add_missing_imports = false,
    },
  },
  eslint_imports = {
    formatter = "eslint",
    eslint_fix_on_save = true,
    save_actions = {
      eslint = true,
      organize_imports = true,
      remove_unused = false,
      add_missing_imports = false,
    },
  },
  conform = {
    formatter = "conform",
    eslint_fix_on_save = false,
    save_actions = {
      eslint = false,
      organize_imports = false,
      remove_unused = false,
      add_missing_imports = false,
    },
  },
}

local ORDER = { "eslint", "eslint_imports", "conform" }

local state = {
  mode = nil,
}

local function load_user_frontend()
  local user_lang = require("core.util").require_if_exists("user.lang") or {}
  return vim.tbl_deep_extend("force", {}, DEFAULTS, user_lang.frontend or {})
end

local function derive_mode(frontend)
  if frontend.formatter == "conform" then return "conform" end
  if frontend.save_actions and frontend.save_actions.organize_imports then return "eslint_imports" end
  return "eslint"
end

function M.available_modes()
  return vim.deepcopy(ORDER)
end

function M.get()
  local frontend = load_user_frontend()
  local mode = state.mode or derive_mode(frontend)
  local preset = PRESETS[mode]
  if preset then frontend = vim.tbl_deep_extend("force", frontend, preset) end
  frontend.mode = mode
  return frontend
end

function M.get_mode()
  return M.get().mode
end

function M.describe(mode)
  local current = mode or M.get_mode()
  if current == "eslint_imports" then return "eslint+imports" end
  return current
end

function M.set_mode(mode)
  if not PRESETS[mode] then return false end
  state.mode = mode
  return true
end

function M.cycle_mode()
  local current = M.get_mode()
  local index = 1
  for i, mode in ipairs(ORDER) do
    if mode == current then
      index = i
      break
    end
  end
  local next_mode = ORDER[(index % #ORDER) + 1]
  state.mode = next_mode
  return next_mode
end

return M
