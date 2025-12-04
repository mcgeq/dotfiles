-- Lazy.nvim 配置
-- AstroNvim 插件管理器配置

require("lazy").setup({
  -- ===== AstroNvim 核心 =====
  {
    "AstroNvim/AstroNvim",
    version = "^5", -- 使用稳定版本（移除此行使用 nightly）
    import = "astronvim.plugins",
    opts = {
      -- Leader 键配置（必须在 lazy 之前设置）
      mapleader = " ", -- 空格作为 leader 键
      maplocalleader = ",", -- 逗号作为 local leader 键
      
      -- 界面配置
      icons_enabled = true, -- 启用图标（需要 Nerd Font）
      
      -- 插件管理
      pin_plugins = nil, -- 自动根据版本固定插件
      update_notifications = true, -- 更新通知
    },
  },
  
  -- ===== 导入配置模块 =====
  { import = "community" }, -- 社区插件（优化版，支持预设系统）
  { import = "plugins" }, -- 自定义插件
} --[[@as LazySpec]], {
  -- ===== Lazy.nvim 全局配置 =====
  
  -- 安装配置
  install = {
    colorscheme = { "astrotheme", "habamax" }, -- 安装时使用的主题
    missing = true, -- 自动安装缺失的插件
  },
  
  -- UI 配置
  ui = {
    backdrop = 100, -- 背景透明度
    border = "rounded", -- 边框样式
    size = { width = 0.8, height = 0.8 }, -- 窗口大小
  },
  
  -- 性能优化配置
  performance = {
    cache = {
      enabled = true, -- 启用缓存
    },
    reset_packpath = true, -- 重置 packpath
    rtp = {
      reset = true, -- 重置 runtimepath
      paths = {}, -- 额外的 runtime 路径
      -- 禁用不必要的内置插件（性能优化）
      disabled_plugins = {
        "gzip", -- gz 文件
        "matchit", -- % 匹配增强（使用 treesitter 代替）
        "matchparen", -- 括号匹配（使用 treesitter 代替）
        "netrwPlugin", -- netrw 文件浏览器（使用 mini.files 代替）
        "tarPlugin", -- tar 文件
        "tohtml", -- 转换为 HTML
        "tutor", -- 教程
        "zipPlugin", -- zip 文件
      },
    },
  },
  
  -- 开发配置
  dev = {
    path = "~/projects", -- 本地插件开发路径
    fallback = true, -- 本地找不到时从 git 获取
  },
  
  -- 更新检查
  checker = {
    enabled = false, -- 禁用自动检查更新（避免启动延迟）
    notify = false, -- 禁用更新通知
  },
  
  -- 变更检测
  change_detection = {
    enabled = true, -- 启用配置文件变更检测
    notify = false, -- 禁用变更通知（减少干扰）
  },
} --[[@as LazyConfig]])
