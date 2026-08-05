local M = {}

local function git_head_fallback()
  local head = vim.b.gitsigns_head
  if head and head ~= "" then return head end
  local root = vim.b.gitsigns_status_dict
  if not root or not root.gitdir then return "" end
  local f = io.open(root.gitdir .. "/HEAD", "r")
  if not f then return "" end
  local line = f:read "*l"
  f:close()
  return line and line:match "^ref: refs/heads/(.+)$" or ""
end

local function short_filename()
  local path = vim.fn.expand "%:p:~"
  if path == "" then return "[No Name]" end
  local segments = vim.split(path, "[\\\\/]")
  local name = segments[#segments]
  local parent = segments[#segments - 1]
  if not parent then return name end
  return parent .. "/" .. name
end

function M.setup()
  local ok, lualine = pcall(require, "lualine")
  if not ok then return end

  lualine.setup {
    options = {
      theme = "auto",
      component_separators = { left = "", right = "" },
      section_separators = { left = "", right = "" },
      globalstatus = true,
      disabled_filetypes = { statusline = { "dashboard", "alpha", "NvimTree" } },
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = {
        {
          function() return git_head_fallback() end,
          cond = function() return vim.b.gitsigns_head ~= nil end,
          icon = "",
          color = { gui = "bold" },
        },
        { "diff", source = "gitsigns" },
      },
      lualine_c = { { function() return short_filename() end } },
      lualine_x = {
        {
          "diagnostics",
          sources = { "nvim_lsp" },
          symbols = { error = "", warn = "", info = "", hint = "" },
        },
        { "encoding" },
      },
      lualine_y = { "progress", "location" },
      lualine_z = { { "filetype", colored = false, icon_only = true } },
    },
    extensions = {},
  }
end

return M
