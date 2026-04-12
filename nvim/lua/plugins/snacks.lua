local M = {}

function M.setup()
  local ok, snacks = pcall(require, "snacks")
  if not ok then return end

  snacks.setup({
    bigfile = {},
    dashboard = {
      preset = {
        header = table.concat({
          [[ /\/\    ___   __ _   /\ \ \__   __(_) _ __ ___ ]],
          [[/    \  / __| / _` | /  \/ /\ \ / /| || '_ ` _ \]],
          [[/ /\/\ \| (__ | (_| |/ /\  /  \ V / | || | | | | |]],
          [[\/    \/ \___| \__, |\_\ \/    \_/  |_||_| |_| |_|]],
          [[               |___/                               ]],
        }, "\n"),
        keys = {
          {
            key = "f",
            icon = " ",
            desc = "Find Files",
            action = function() snacks.picker.files() end,
          },
          {
            key = "r",
            icon = " ",
            desc = "Recent Files",
            action = function() snacks.picker.recent() end,
          },
          {
            key = "c",
            icon = " ",
            desc = "Config",
            action = "<cmd>NvimConfig<cr>",
          },
          {
            key = "m",
            icon = " ",
            desc = "Mason",
            action = "<cmd>Mason<cr>",
          },
          {
            key = "q",
            icon = " ",
            desc = "Quit",
            action = "<cmd>qa<cr>",
          },
        },
      },
      sections = {
        { section = "header" },
        { section = "keys", gap = 1, padding = 1 },
        { icon = " ", title = "Recent Files", section = "recent_files", indent = 2, padding = 1 },
        { icon = " ", title = "Projects", section = "projects", indent = 2, padding = 1 },
      },
    },
    explorer = {},
    image = {},
    indent = {},
    input = {},
    lazygit = {},
    notifier = {
      enabled = true,
      timeout = 3000,
    },
    picker = {
      matcher = { frecency = true, case_mode = "smart_case" },
      layout = {
        preset = "default",
      },
      formatters = {
        file = {
          filename_first = true,
        },
      },
      files = {
        hidden = false,
        follow = true,
        exclude = {
          ".git",
          "node_modules",
          "dist",
          "build",
          "target",
          ".next",
          ".cache",
        },
      },
      win = {
        input = {
          keys = {
            ["<Esc>"] = { "close", mode = { "n", "i" } },
            ["<C-e>"] = { "list_down", mode = { "n", "i" } },
            ["<C-u>"] = { "list_up", mode = { "n", "i" } },
          },
        },
      },
    },
    scope = {},
    statuscolumn = {},
  })

  local map = require("core.keymaps").map
  map("n", "<leader><space>", function() snacks.picker.smart() end, "Smart find files")
  map("n", "<leader>,", function() snacks.picker.buffers() end, "Find buffers")
  map("n", "<leader>/", function() snacks.picker.grep() end, "Grep project")
  map("n", "<leader>e", function() snacks.explorer() end, "Explorer")

  map("n", "<leader>ff", function() snacks.picker.files() end, "Find files")
  map("n", "<leader>fg", function() snacks.picker.git_files() end, "Find git files")
  map("n", "<leader>fr", function() snacks.picker.recent() end, "Recent files")
  map("n", "<leader>fc", function() snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, "Find config files")

  map("n", "<leader>sb", function() snacks.picker.lines() end, "Buffer lines")
  map({ "n", "x" }, "<leader>sw", function() snacks.picker.grep_word() end, "Search word")
  map("n", "<leader>sd", function() snacks.picker.diagnostics() end, "Project diagnostics")
  map("n", "<leader>sD", function() snacks.picker.diagnostics_buffer() end, "Buffer diagnostics")
  map("n", "<leader>sk", function() snacks.picker.keymaps() end, "Keymaps")
  map("n", "<leader>sh", function() snacks.picker.help() end, "Help")
  map("n", "<leader>sn", function() snacks.picker.notifications() end, "Notifications")
  map("n", "<leader>sR", function() snacks.picker.resume() end, "Resume search")
  map("n", "<leader>ss", function() snacks.picker.lsp_symbols() end, "LSP symbols")
  map("n", "<leader>sS", function() snacks.picker.lsp_workspace_symbols() end, "Workspace symbols")
  map("n", "<leader>sm", function() snacks.picker.marks() end, "Marks")
  map("n", "<leader>sc", function() snacks.picker.commands() end, "Commands")
  map("n", "<leader>sp", function() snacks.picker.files({ cwd = vim.fn.stdpath("data") .. "/site/pack" }) end, "Plugin files")

  map("n", "<leader>gb", function() snacks.picker.git_branches() end, "Git branches")
  map("n", "<leader>gg", function()
    if vim.fn.executable("lazygit") == 1 then
      snacks.lazygit()
      return
    end
    vim.notify("`lazygit` is not available on PATH.", vim.log.levels.WARN, { title = "LazyGit" })
  end, "LazyGit")
  map("n", "<leader>gs", function() snacks.picker.git_status() end, "Git status")
  map("n", "<leader>gl", function() snacks.picker.git_log() end, "Git log")
  map("n", "<leader>gf", function() snacks.picker.git_log_file() end, "Git file log")
  map("n", "<leader>gL", function() snacks.picker.git_log_line() end, "Git line log")

  map("n", "gd", function() snacks.picker.lsp_definitions() end, "Goto definition")
  map("n", "gD", function() snacks.picker.lsp_declarations() end, "Goto declaration")
  map("n", "gr", function() snacks.picker.lsp_references() end, "Goto references", { nowait = true })
  map("n", "gI", function() snacks.picker.lsp_implementations() end, "Goto implementation")
  map("n", "gy", function() snacks.picker.lsp_type_definitions() end, "Goto type definition")
  map("n", "<leader>uC", function() snacks.picker.colorschemes() end, "Colorschemes")
end

return M
