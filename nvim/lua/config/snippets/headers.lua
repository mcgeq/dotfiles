-- 文件头 Snippet 生成器
-- 为不同文件类型生成统一的文件头模板

local constants = require("config.constants")
local utils = require("config.snippets.utils")

local M = {}

--- 创建标准文件头 snippet（适用于单行注释）
---@param comment_prefix string 注释前缀（如 "//" 或 "#"）
---@return table snippet LuaSnip snippet 对象
function M.create_line_comment_header(comment_prefix)
  local ls = require("luasnip")
  local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node
  local user = constants.user

  return s("header", {
    t(utils.create_separator(comment_prefix)),
    t({ "", comment_prefix .. " Copyright (C) " }),
    f(utils.get_current_year),
    t(" " .. user.name .. ". All rights reserved."),
    t({ "", comment_prefix .. " Author:         " .. user.name }),
    t({ "", comment_prefix .. " Email:          <" .. user.email .. ">" }),
    t({ "", comment_prefix .. " File:           " }),
    f(utils.get_filename),
    t({ "", comment_prefix .. " Description:    " }),
    i(1, "Description of the file"),
    t({ "", comment_prefix .. " Create   Date:  " }),
    f(utils.get_current_datetime),
    t({ "", comment_prefix .. " Last Modified:  " }),
    f(utils.get_current_datetime),
    t({ "", comment_prefix .. " Modified   By:  " .. user.name .. " <" .. user.email .. ">" }),
    t({ "", utils.create_separator(comment_prefix) }),
    t({ "", "" }),
    i(0),
  })
end

--- 创建块注释文件头（如 HTML, CSS）
---@param start_comment string 开始注释符（如 "<!--" 或 "/*"）
---@param end_comment string 结束注释符（如 "-->" 或 "*/"）
---@param line_prefix string|nil 每行前缀（可选）
---@return table snippet LuaSnip snippet 对象
function M.create_block_comment_header(start_comment, end_comment, line_prefix)
  local ls = require("luasnip")
  local s, t, i, f = ls.snippet, ls.text_node, ls.insert_node, ls.function_node
  local user = constants.user
  local prefix = line_prefix or " "

  return s("header", {
    t(start_comment .. " " .. string.rep("-", 77)),
    t({ "", prefix .. "Copyright (C) " }),
    f(utils.get_current_year),
    t(" " .. user.name .. ". All rights reserved."),
    t({ "", prefix .. "Author:         " .. user.name }),
    t({ "", prefix .. "Email:          <" .. user.email .. ">" }),
    t({ "", prefix .. "File:           " }),
    f(utils.get_filename),
    t({ "", prefix .. "Description:    " }),
    i(1, "Description of the file"),
    t({ "", prefix .. "Create   Date:  " }),
    f(utils.get_current_datetime),
    t({ "", prefix .. "Last Modified:  " }),
    f(utils.get_current_datetime),
    t({ "", prefix .. "Modified   By:  " .. user.name .. " <" .. user.email .. ">" }),
    t({ "", prefix .. string.rep("-", 77) .. " " .. end_comment }),
    t({ "", "" }),
    i(0),
  })
end

return M
