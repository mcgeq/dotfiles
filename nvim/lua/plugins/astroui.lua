-- AstroUI 配置
-- 管理 UI 外观、主题、图标等
-- 文档: `:h astroui`

---@type LazySpec
return {
  "AstroNvim/astroui",
  ---@type AstroUIOpts
  opts = {
    -- ===== 主题配置 =====
    colorscheme = "astrodark", -- 默认主题（可选：astrotheme, catppuccin, tokyonight 等）

    -- ===== 高亮组配置 =====
    highlights = {
      -- 全局高亮覆盖（应用于所有主题）
      init = {
        -- 示例：修改背景色
        -- Normal = { bg = "#000000" },
        -- 高亮当前行号
        -- CursorLineNr = { fg = "#ffcc00", bold = true },
      },
      
      -- 特定主题的高亮覆盖
      astrodark = {
        -- 自定义 astrodark 主题的高亮
        -- Normal = { bg = "#1a1a2e" },
      },
    },

    -- ===== 图标配置 =====
    icons = {
      -- LSP 加载动画
      LSPLoading1 = "⠋",
      LSPLoading2 = "⠙",
      LSPLoading3 = "⠹",
      LSPLoading4 = "⠸",
      LSPLoading5 = "⠼",
      LSPLoading6 = "⠴",
      LSPLoading7 = "⠦",
      LSPLoading8 = "⠧",
      LSPLoading9 = "⠇",
      LSPLoading10 = "⠏",

      -- 可以在这里添加更多自定义图标
      -- Git = "",
      -- GitBranch = "",
      -- GitAdd = "",
      -- GitChange = "",
      -- GitDelete = "",
    },

    -- ===== 状态行配置 =====
    status = {
      -- 覆盖默认的虚拟环境显示组件
      components = {
        -- 自定义 Python 虚拟环境显示（覆盖默认的 env 组件）
        env = function()
          -- 检查是否在 Python 文件中
          if vim.bo.filetype ~= "python" then
            return ""
          end
          
          -- 优先使用全局变量（由 python-venv-auto.lua 设置）
          if vim.g.venv_display_name then
            return " " .. vim.g.venv_display_name
          end
          
          -- 次选：使用 buffer 变量
          if vim.b.venv_name then
            return " " .. vim.b.venv_name
          end
          
          -- 备用：从 VIRTUAL_ENV 环境变量提取名称
          local venv = vim.env.VIRTUAL_ENV
          if venv and venv ~= "" then
            -- 提取虚拟环境名称（最后一个目录）
            local venv_name = venv:match("[\\/]([^\\/]+)$") or "venv"
            return " " .. venv_name
          end
          
          return ""
        end,
      },
    },
  },
}
