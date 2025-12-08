-- LuaSnip snippets 配置
-- 文件头 snippets 定义（重构版本）
-- 使用模块化结构和常量配置

local constants = require("config.constants")
local headers = require("config.snippets.headers")
local neorg_snippets = require("config.snippets.neorg")

local M = {}

-- 为使用单行注释的语言生成 snippets
local line_comment_filetypes = {
  "rust",
  "c",
  "cpp",
  "javascript",
  "typescript",
  "javascriptreact",
  "typescriptreact",
  "lua",
  "go",
  "java",
  "cs",
  "kotlin",
  "swift",
  "dart",
}

-- 为使用单行注释的语言创建 snippets
for _, ft in ipairs(line_comment_filetypes) do
  local comment_style = constants.filetype_to_comment[ft]
  if comment_style then
    local comment_prefix = constants.comment_styles[comment_style]
    M[ft] = { headers.create_line_comment_header(comment_prefix) }
  end
end

-- Python 使用 # 注释
M.python = { headers.create_line_comment_header(constants.comment_styles.hash) }

-- HTML 使用块注释
M.html = { headers.create_block_comment_header("<!--", "-->") }

-- CSS/SCSS 使用块注释
M.css = { headers.create_block_comment_header("/*", "*/") }
M.scss = { headers.create_block_comment_header("/*", "*/") }

-- Neorg snippets
M.norg = neorg_snippets.snippets

return M

