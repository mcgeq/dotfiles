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
    -- ===== 顶级快捷键（最常用）=====
    { "<leader><space>", function() Snacks.picker.smart() end, desc = "Smart Find Files" },
    { "<leader>,", function() Snacks.picker.buffers() end, desc = "Buffers" },
    { "<leader>/", function() Snacks.picker.grep() end, desc = "Grep" },
    { "<leader>e", function() Snacks.explorer() end, desc = "File Explorer" },
    
    -- ===== 查找文件（<leader>f）=====
    { "<leader>fc", function() Snacks.picker.files({ cwd = vim.fn.stdpath("config") }) end, desc = "Config Files" },
    { "<leader>ff", function() Snacks.picker.files() end, desc = "Find Files" },
    { "<leader>fg", function() Snacks.picker.git_files() end, desc = "Git Files" },
    { "<leader>fm", function() require("mini.files").open(vim.api.nvim_buf_get_name(0), true) end, desc = "Mini Files (Current)" },
    { "<leader>fM", function() require("mini.files").open(vim.uv.cwd(), true) end, desc = "Mini Files (CWD)" },
    { "<leader>fp", function() Snacks.picker.projects() end, desc = "Projects" },
    { "<leader>fr", function() Snacks.picker.recent() end, desc = "Recent Files" },
    
    -- ===== Git 操作（<leader>g）=====
    { "<leader>gb", function() Snacks.picker.git_branches() end, desc = "Branches" },
    { "<leader>gd", function() Snacks.picker.git_diff() end, desc = "Diff (Hunks)" },
    { "<leader>gf", function() Snacks.picker.git_log_file() end, desc = "File Log" },
    { "<leader>gl", function() Snacks.picker.git_log() end, desc = "Commit Log" },
    { "<leader>gL", function() Snacks.picker.git_log_line() end, desc = "Line Log" },
    { "<leader>gs", function() Snacks.picker.git_status() end, desc = "Status" },
    { "<leader>gS", function() Snacks.picker.git_stash() end, desc = "Stash" },
    
    -- ===== 搜索（<leader>s）=====
    -- 合并 grep 相关功能，移除重复
    { "<leader>sw", function() Snacks.picker.grep_word() end, desc = "Grep Word/Selection", mode = { "n", "x" } },
    { "<leader>sb", function() Snacks.picker.lines() end, desc = "Buffer Lines" },
    { "<leader>sB", function() Snacks.picker.grep_buffers() end, desc = "Grep Buffers" },
    
    -- 搜索系统相关
    { "<leader>sc", function() Snacks.picker.commands() end, desc = "Commands" },
    { "<leader>sd", function() Snacks.picker.diagnostics() end, desc = "Diagnostics" },
    { "<leader>sD", function() Snacks.picker.diagnostics_buffer() end, desc = "Buffer Diagnostics" },
    { "<leader>sh", function() Snacks.picker.help() end, desc = "Help" },
    { "<leader>sk", function() Snacks.picker.keymaps() end, desc = "Keymaps" },
    { "<leader>sm", function() Snacks.picker.marks() end, desc = "Marks" },
    { "<leader>sn", function() Snacks.picker.notifications() end, desc = "Notifications" },
    { "<leader>sp", function() Snacks.picker.lazy() end, desc = "Plugins" },
    { "<leader>sR", function() Snacks.picker.resume() end, desc = "Resume Last Search" },
    { "<leader>ss", function() Snacks.picker.lsp_symbols() end, desc = "LSP Symbols" },
    { "<leader>sS", function() Snacks.picker.lsp_workspace_symbols() end, desc = "Workspace Symbols" },
    
    -- ===== LSP 导航（g 系列）=====
    { "gd", function() Snacks.picker.lsp_definitions() end, desc = "Goto Definition" },
    { "gD", function() Snacks.picker.lsp_declarations() end, desc = "Goto Declaration" },
    { "gr", function() Snacks.picker.lsp_references() end, nowait = true, desc = "References" },
    { "gI", function() Snacks.picker.lsp_implementations() end, desc = "Goto Implementation" },
    { "gy", function() Snacks.picker.lsp_type_definitions() end, desc = "Goto Type Definition" },
    
    -- ===== 实用工具（<leader>u）=====
    { "<leader>uC", function() Snacks.picker.colorschemes() end, desc = "Colorschemes" },
  },
}