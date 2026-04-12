local M = {}

local THEME_ORDER = { "tokyonight", "catppuccin", "kanagawa" }
local FALLBACK_THEME_ORDER = { "catppuccin", "kanagawa", "tokyonight" }
local session_theme = nil

local function setup_catppuccin(overrides)
  local ok, catppuccin = pcall(require, "catppuccin")
  if not ok then return end

  local defaults = {
    flavour = "mocha",
    transparent_background = false,
    show_end_of_buffer = false,
    term_colors = true,
    dim_inactive = {
      enabled = true,
      shade = "dark",
      percentage = 0.15,
    },
    integrations = {
      cmp = true,
      gitsigns = true,
      mini = true,
      mason = true,
      noice = true,
      notify = false,
      treesitter = true,
      which_key = true,
      blink_cmp = true,
      snacks = true,
    },
  }

  catppuccin.setup(vim.tbl_deep_extend("force", defaults, overrides or {}))
end

local function setup_tokyonight(overrides)
  local ok, tokyonight = pcall(require, "tokyonight")
  if not ok then return end

  local defaults = {
    style = "night",
    transparent = false,
    terminal_colors = true,
    dim_inactive = true,
    lualine_bold = true,
    styles = {
      comments = { italic = true },
      keywords = { italic = true },
      sidebars = "dark",
      floats = "dark",
    },
    sidebars = { "qf", "help", "terminal" },
    on_highlights = function(hl, colors)
      hl.CursorLineNr = { fg = colors.orange, bold = true }
    end,
  }

  tokyonight.setup(vim.tbl_deep_extend("force", defaults, overrides or {}))
end

local function setup_kanagawa(overrides)
  local ok, kanagawa = pcall(require, "kanagawa")
  if not ok then return end

  local defaults = {
    compile = false,
    undercurl = true,
    commentStyle = { italic = true },
    keywordStyle = { italic = true },
    statementStyle = { bold = true },
    transparent = false,
    dimInactive = true,
    terminalColors = true,
    theme = "wave",
    background = {
      dark = "wave",
      light = "lotus",
    },
  }

  kanagawa.setup(vim.tbl_deep_extend("force", defaults, overrides or {}))
end

local function resolve_theme_name(user, requested)
  if requested and requested ~= "" then return requested end
  if session_theme and session_theme ~= "" then return session_theme end
  local theme = user.theme or {}
  return theme.name or user.colorscheme or "tokyonight"
end

local function theme_exists(name)
  local lua_theme = vim.api.nvim_get_runtime_file("colors/" .. name .. ".lua", true)
  if #lua_theme > 0 then return true end

  local vim_theme = vim.api.nvim_get_runtime_file("colors/" .. name .. ".vim", true)
  return #vim_theme > 0
end

local function setup_theme(name, overrides)
  if name == "catppuccin" then
    setup_catppuccin(overrides.catppuccin)
  elseif name == "kanagawa" then
    setup_kanagawa(overrides.kanagawa)
  elseif name == "tokyonight" then
    setup_tokyonight(overrides.tokyonight)
  end
end

local function resolve_fallback_theme(primary)
  if primary and theme_exists(primary) then return primary end

  for _, name in ipairs(FALLBACK_THEME_ORDER) do
    if name ~= primary and theme_exists(name) then return name end
  end

  return primary or FALLBACK_THEME_ORDER[1]
end

local function complete_theme_names(arg_lead)
  local seen = {}
  local names = {}

  for _, name in ipairs(THEME_ORDER) do
    if not arg_lead or arg_lead == "" or name:find(arg_lead, 1, true) == 1 then
      seen[name] = true
      table.insert(names, name)
    end
  end

  for _, name in ipairs(vim.fn.getcompletion(arg_lead or "", "color")) do
    if not seen[name] then
      seen[name] = true
      table.insert(names, name)
    end
  end

  table.sort(names)
  return names
end

function M.apply_theme(requested)
  local user = require("user")
  local theme = vim.deepcopy(user.theme or {})
  local name = resolve_theme_name(user, requested)
  local overrides = theme.overrides or {}
  local active_name = resolve_fallback_theme(name)

  setup_theme(active_name, overrides)

  local ok_colorscheme, err = pcall(vim.cmd.colorscheme, active_name)
  if not ok_colorscheme then
    vim.schedule(function()
      local message
      if active_name ~= name then
        message = ("Theme `%s` is not available yet, falling back to `%s`."):format(name, active_name)
      else
        message = ("Theme `%s` is not installed or not loaded yet. Run `:PackUpdate`, `:PackRepair`, or restart Neovim."):format(name)
      end
      vim.notify(message, vim.log.levels.WARN)
      if err then vim.notify(tostring(err), vim.log.levels.DEBUG, { title = "Theme Fallback" }) end
    end)
  else
    session_theme = active_name
  end

  if theme.highlights then
    if type(theme.highlights) == "function" then
      local ok, result = pcall(theme.highlights, active_name)
      if ok and type(result) == "table" then
        for group, value in pairs(result) do
          vim.api.nvim_set_hl(0, group, value)
        end
      end
    elseif type(theme.highlights) == "table" then
      for group, value in pairs(theme.highlights) do
        vim.api.nvim_set_hl(0, group, value)
      end
    end
  end

  return ok_colorscheme, active_name
end

function M.available_themes(arg_lead)
  return complete_theme_names(arg_lead)
end

function M.current_theme()
  return vim.g.colors_name or session_theme or resolve_theme_name(require("user"))
end

function M.cycle_theme()
  local current = M.current_theme()
  local index = 0
  for i, name in ipairs(THEME_ORDER) do
    if name == current then
      index = i
      break
    end
  end
  local next_theme = THEME_ORDER[(index % #THEME_ORDER) + 1]
  M.apply_theme(next_theme)
  return next_theme
end

function M.setup()
  M.apply_theme()

  local ok_which_key, which_key = pcall(require, "which-key")
  if ok_which_key then
    which_key.setup({
      delay = 200,
      preset = "helix",
      spec = {
        { "<leader>b", group = "buffer" },
        { "<leader>c", group = "code" },
        { "<leader>d", group = "database" },
        { "<leader>f", group = "find" },
        { "<leader>g", group = "git" },
        { "<leader>j", group = "jujutsu" },
        { "<leader>l", group = "local" },
        { "<leader>p", group = "plugins" },
        { "<leader>q", group = "quit" },
        { "<leader>s", group = "search" },
        { "<leader>u", group = "ui" },
        { "<leader>w", group = "window" },
        { "<leader>x", group = "diagnostics" },
      },
    })
  end

  local ok_bufferline, bufferline = pcall(require, "bufferline")
  if ok_bufferline then
    bufferline.setup({
      options = {
        always_show_bufferline = true,
        diagnostics = "nvim_lsp",
      },
    })
  end
end

return M
