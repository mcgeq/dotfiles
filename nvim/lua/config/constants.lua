-- 全局常量配置
-- 集中管理个人信息、路径和其他常量，便于维护和共享配置

---@class UserInfo
---@field name string 用户名
---@field email string 邮箱地址
---@field github string GitHub 用户名

---@class Constants
---@field user UserInfo 用户信息
---@field comment_styles table<string, string> 注释风格映射
local M = {}

-- 用户信息（修改这里即可更新所有引用）
M.user = {
  name = "mcge",
  email = "mcgeq@outlook.com",
  github = "mcgeq",
}

-- 注释风格定义
M.comment_styles = {
  line = "//", -- C, C++, Rust, JS, TS, Java, C#, Go
  hash = "#", -- Python, Shell, Ruby
  dash = "--", -- Lua, SQL, Haskell
  semicolon = ";", -- Lisp, Scheme
}

-- 文件类型到注释风格的映射
M.filetype_to_comment = {
  -- Line style (//)
  c = "line",
  cpp = "line",
  rust = "line",
  javascript = "line",
  typescript = "line",
  javascriptreact = "line",
  typescriptreact = "line",
  java = "line",
  cs = "line",
  go = "line",
  kotlin = "line",
  swift = "line",
  dart = "line",
  
  -- Hash style (#)
  python = "hash",
  sh = "hash",
  bash = "hash",
  zsh = "hash",
  ruby = "hash",
  perl = "hash",
  yaml = "hash",
  toml = "hash",
  
  -- Dash style (--)
  lua = "dash",
  sql = "dash",
  haskell = "dash",
  
  -- Semicolon style (;)
  lisp = "semicolon",
  scheme = "semicolon",
}

-- 支持的文件类型列表（用于时间戳自动更新）
M.timestamp_filetypes = {
  "rust",
  "c",
  "cpp",
  "python",
  "typescript",
  "javascript",
  "typescriptreact",
  "javascriptreact",
  "cs",
  "java",
  "go",
  "lua",
  "css",
  "scss",
  "html",
  "kotlin",
  "swift",
  "dart",
}

return M
