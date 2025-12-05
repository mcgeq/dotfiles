-- AstroCore 配置
-- 统一管理 vim 选项、键映射、自动命令等核心配置
-- 文档: `:h astrocore`

---@type LazySpec
return {
  "AstroNvim/astrocore",
  ---@type AstroCoreOpts
  opts = {
    -- ===== 核心特性配置 =====
    features = {
      large_buf = { size = 1024 * 500, lines = 10000 }, -- 大文件阈值（500KB）
      autopairs = true, -- 自动括号配对
      cmp = true, -- 自动补全
      diagnostics = { virtual_text = true, virtual_lines = false }, -- 诊断显示方式
      highlighturl = true, -- 高亮 URL
      notifications = true, -- 通知系统
    },

    -- ===== 诊断配置 =====
    diagnostics = {
      virtual_text = true, -- 在行尾显示诊断信息
      underline = true, -- 下划线标记诊断
      signs = true, -- 侧边栏显示诊断标记
      update_in_insert = false, -- 插入模式不更新诊断（性能优化）
      severity_sort = true, -- 按严重性排序
    },

    -- ===== 文件类型配置 =====
    filetypes = {
      -- 自定义文件类型识别
      extension = {
        -- foo = "fooscript",
      },
      filename = {
        -- [".foorc"] = "fooscript",
      },
      pattern = {
        -- [".*/etc/foo/.*"] = "fooscript",
      },
    },

    -- ===== Vim 选项配置 =====
    options = {
      opt = {
        -- 编辑器外观
        relativenumber = true, -- 相对行号
        number = true, -- 显示行号
        signcolumn = "yes", -- 始终显示标志列
        wrap = false, -- 不自动换行
        cursorline = true, -- 高亮当前行
        colorcolumn = "120", -- 120列标尺

        -- 编辑行为
        spell = false, -- 拼写检查
        expandtab = true, -- 使用空格替代 Tab
        shiftwidth = 2, -- 缩进宽度
        tabstop = 2, -- Tab 宽度
        scrolloff = 8, -- 滚动时保持 8 行上下文
        sidescrolloff = 8, -- 水平滚动时保持 8 列上下文

        -- 性能优化
        updatetime = 250, -- CursorHold 触发时间（ms）- 平衡性能和响应速度
        timeoutlen = 300, -- 快捷键等待时间（ms）
        
        -- 搜索
        ignorecase = true, -- 搜索忽略大小写
        smartcase = true, -- 智能大小写搜索

        -- 文件处理
        undofile = true, -- 持久化撤销
        swapfile = false, -- 禁用交换文件
        backup = false, -- 禁用备份文件
      },
      g = {
        -- 全局变量配置
        -- mapleader 和 maplocalleader 在 lazy_setup.lua 中配置
      },
    },

    -- ===== 键映射配置 =====
    mappings = {
      -- Normal 模式
      n = {
        -- 文件保存（从 mappings.lua 合并）
        ["<C-s>"] = { ":w!<CR>", desc = "Save File" },

        -- Buffer 导航
        ["]b"] = { function() require("astrocore.buffer").nav(vim.v.count1) end, desc = "Next buffer" },
        ["[b"] = { function() require("astrocore.buffer").nav(-vim.v.count1) end, desc = "Previous buffer" },

        -- Buffer 关闭
        ["<Leader>bd"] = {
          function()
            require("astroui.status.heirline").buffer_picker(
              function(bufnr) require("astrocore.buffer").close(bufnr) end
            )
          end,
          desc = "Close buffer from tabline",
        },

        -- 窗口导航优化
        ["<C-h>"] = { "<C-w>h", desc = "Move to left window" },
        ["<C-j>"] = { "<C-w>j", desc = "Move to window below" },
        ["<C-k>"] = { "<C-w>k", desc = "Move to window above" },
        ["<C-l>"] = { "<C-w>l", desc = "Move to right window" },

        -- 快速退出
        ["<Leader>q"] = { ":q<CR>", desc = "Quit" },
        ["<Leader>Q"] = { ":qa!<CR>", desc = "Quit all without saving" },
      },

      -- Insert 模式
      i = {
        -- 文件保存（从 mappings.lua 合并）
        ["<C-s>"] = { "<Esc>:w!<CR>", desc = "Save File" },

        -- 快速退出插入模式
        ["jk"] = { "<Esc>", desc = "Exit insert mode" },
      },

      -- Visual 模式
      v = {
        -- 缩进后保持选择
        ["<"] = { "<gv", desc = "Indent left" },
        [">"] = { ">gv", desc = "Indent right" },
      },

      -- Terminal 模式
      t = {
        -- 终端模式快捷键
        ["<Esc><Esc>"] = { "<C-\\><C-n>", desc = "Exit terminal mode" },
      },
    },
  },
}
