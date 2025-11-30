return {
  "folke/snacks.nvim",
  opts = function(_, opts)
    opts = opts or {}
    
    -- ============================================
    -- Picker 优化配置
    -- ============================================
    opts.picker = opts.picker or {}
    -- 性能优化
    opts.picker.throttle = 20 -- 降低延迟，提升响应速度
    opts.picker.follow = true -- 自动跟随光标
    
    -- 窗口布局优化
    opts.picker.layout = {
      preset = "default", -- 使用默认布局（居中显示）
      -- 可选布局: "default" | "ivy" | "dropdown" | "cursor"
    }
    
    -- 预览窗口配置
    opts.picker.preview = {
      enabled = true,
      width = 0.5, -- 预览窗口占 50% 宽度
      border = "rounded", -- 圆角边框
    }
    
    -- 搜索行为优化
    opts.picker.matcher = {
      frecency = true, -- 启用频率+最近使用排序（智能排序）
      case_mode = "smart_case", -- 智能大小写（小写忽略大小写，大写精确匹配）
    }
    
    -- Git 集成优化
    opts.picker.formatters = {
      file = {
        filename_first = true, -- 文件名优先显示（更易扫描）
      },
    }
    
    -- 性能：排除大文件和目录
    opts.picker.files = {
      hidden = false, -- 默认不显示隐藏文件（需要时可用 <C-h> 切换）
      follow = true, -- 跟随符号链接
      ignore_patterns = {
        "node_modules",
        ".git",
        "dist",
        "build",
        "target",
        ".next",
        ".cache",
      },
    }
    
    opts.explorer = opts.explorer or {}
    -- Dashboard configuration
    opts.dashboard = opts.dashboard or {}
    opts.dashboard.preset = opts.dashboard.preset or {}
    opts.dashboard.preset.header = table.concat({
      " /\\/\\    ___   __ _   /\\ \\ \\__   __(_) _ __ ___ ",
      "/    \\  / __| / _` | /  \\/ /\\ \\ / /| || '_ ` _ \\",
      "/ /\\/\\ \\| (__ | (_| |/ /\\  /  \\ V / | || | | | | |",
      "\\/    \\/ \\___| \\__, |\\_\\ \\/    \\_/  |_||_| |_| |_|",
      "               |___/                               ",
    }, "\n")
    opts.dashboard.preset.keys = vim.list_extend(opts.dashboard.preset.keys or {}, {
      {
        key = "c",
        icon = "⚙ ",
        desc = "Config       ",
        action = function()
          local ok, snacks = pcall(require, "snacks")
          if ok and snacks and snacks.picker then
            snacks.picker.files {
              cwd = vim.fn.stdpath "config",
              prompt_title = " Neovim Config",
              hidden = true,
            }
          end
        end,
      },
      {
        key = "l",
        icon = "󰒲",
        desc = "Lazy         ",
        action = "<cmd>Lazy<CR>",
      },
      {
        key = "m",
        icon = "🔧",
        desc = "Mason        ",
        action = "<cmd>Mason<CR>",
      },
      {
        key = "q",
        icon = "󰗼",
        desc = "Quit         ",
        action = "<cmd>qa<CR>",
      },
    })
    return opts
  end,
  -- Note: Using Snacks global variable here is safe in lazy.nvim keys configuration
  -- because the plugin loads before keys are bound. The global is guaranteed to exist.
  keys = {
    -- Top Pickers & Explorer
    { "<leader><space>", function() Snacks.picker.smart() end, desc = "Smart Find Files" },
    { "<leader>,", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>/", function() Snacks.picker.grep() end, desc = "Grep" },
    { "<leader>:", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>n", function() Snacks.picker.notifications() end, desc = "Notification History" },
    { "<leader>e", function() Snacks.explorer() end, desc = "File Explorer" },
    -- find
    { "<leader>fb", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Find Config File" },
    { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files" },
    { "<leader>fg", function() Snacks.picker.git_files() end, desc = "Find Git Files" },
    { "<leader>fm", function() require("mini.files").open(vim.api.nvim_buf_get_name(0), true) end, desc = "Open mini.files (Directory of Current File)" },
    { "<leader>fM", function() require("mini.files").open(vim.uv.cwd(), true) end, desc = "Open mini.files (cwd)" },
    { "<leader>fp", function() Snacks.picker.projects() end, desc = "Projects" },
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent" },
    -- git
    { "<leader>gb", function() Snacks.picker.git_branches() end, desc = "Git Branches" },
    { "<leader>gl", function() Snacks.picker.git_log() end, desc = "Git Log" },
    { "<leader>gL", function() Snacks.picker.git_log_line() end, desc = "Git Log Line" },
    { "<leader>gs", function() Snacks.picker.git_status() end, desc = "Git Status" },
    { "<leader>gS", function() Snacks.picker.git_stash() end, desc = "Git Stash" },
    { "<leader>gd", function() Snacks.picker.git_diff() end, desc = "Git Diff (Hunks)" },
    { "<leader>gf", function() Snacks.picker.git_log_file() end, desc = "Git Log File" },
    -- Grep
    { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
    { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Open Buffers" },
    { "<leader>sg", function() Snacks.picker.grep() end, desc = "Grep" },
    { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Visual selection or word", mode = { "n", "x" } },
    -- search
    { '<leader>s"', function() Snacks.picker.registers() end, desc = "Registers" },
    { '<leader>s/', function() Snacks.picker.search_history() end, desc = "Search History" },
    { "<leader>sa", function() Snacks.picker.autocmds() end, desc = "Autocmds" },
    { "<leader>sc", function() Snacks.picker.command_history() end, desc = "Command History" },
    { "<leader>sC", function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
    { "<leader>sD", function() Snacks.picker.diagnostics_buffer() end, desc = "Buffer Diagnostics" },
    { "<leader>sh", function() Snacks.picker.help() end, desc = "Help Pages" },
    { "<leader>sH", function() Snacks.picker.highlights() end, desc = "Highlights" },
    { "<leader>si", function() Snacks.picker.icons() end, desc = "Icons" },
    { "<leader>sj", function() Snacks.picker.jumps() end, desc = "Jumps" },
    { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>sl", function() Snacks.picker.loclist() end, desc = "Location List" },
    { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
    { "<leader>sM", function() Snacks.picker.man() end, desc = "Man Pages" },
    { "<leader>sp", function() Snacks.picker.lazy() end, desc = "Search for Plugin Spec" },
    { "<leader>sq", function() Snacks.picker.qflist() end, desc = "Quickfix List" },
    { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume" },
    { "<leader>su", function() Snacks.picker.undo() end, desc = "Undo History" },
    { "<leader>uC", function() Snacks.picker.colorschemes() end, desc = "Colorschemes" },
    -- LSP
    { "gd", function() Snacks.picker.lsp_definitions() end, desc = "Goto Definition" },
    { "gD", function() Snacks.picker.lsp_declarations() end, desc = "Goto Declaration" },
    { "gr", function() Snacks.picker.lsp_references() end, nowait = true, desc = "References" },
    { "gI", function() Snacks.picker.lsp_implementations() end, desc = "Goto Implementation" },
    { "gy", function() Snacks.picker.lsp_type_definitions() end, desc = "Goto T[y]pe Definition" },
    { "<leader>ss", function() Snacks.picker.lsp_symbols() end, desc = "LSP Symbols" },
    { "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end, desc = "LSP Workspace Symbols" },
  },
}