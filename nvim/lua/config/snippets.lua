-- LuaSnip snippets 配置
-- 文件头 snippets 定义

local luasnip = require "luasnip"
local s = luasnip.snippet
local t = luasnip.text_node
local i = luasnip.insert_node
local f = luasnip.function_node

local function get_filename()
  return vim.fn.expand "%:t"
end

local function get_current_date_time()
  return os.date "%Y-%m-%d %H:%M:%S"
end

local function get_current_year()
  return os.date "%Y"
end

-- 通用的文件头模板
local function create_file_header(comment_prefix)
  return s(
    "header",
    {
      t { comment_prefix .. " -----------------------------------------------------------------------------" },
      t { comment_prefix .. "    Copyright (C) " },
      f(get_current_year, {}),
      t { " mcge. All rights reserved." },
      t { "", comment_prefix .. " Author:         mcge" },
      t { "", comment_prefix .. " Email:          <mcgeq@outlook.com>" },
      t { "", comment_prefix .. " File:           " },
      f(get_filename, {}),
      t { "", comment_prefix .. " Description:    " },
      i(1, "Description of the file"),
      t { "", comment_prefix .. " Create   Date:  " },
      f(get_current_date_time, {}),
      t { "", comment_prefix .. " Last Modified:  " },
      f(get_current_date_time, {}),
      t { "", comment_prefix .. " Modified   By:  mcgeq <mcgeq@outlook.com>" },
      t { "", comment_prefix .. " -----------------------------------------------------------------------------" },
      t { "" },
    }
  )
end

return {
  -- Rust 文件头 (使用 // 注释)
  rust = {
    create_file_header "//",
  },
  -- C/C++ 文件头 (使用 // 注释)
  c = {
    create_file_header "//",
  },
  cpp = {
    create_file_header "//",
  },
  -- Python 文件头 (使用 # 注释)
  python = {
    create_file_header "#",
  },
  -- TypeScript/JavaScript 文件头 (使用 // 注释)
  typescript = {
    create_file_header "//",
  },
  javascript = {
    create_file_header "//",
  },
  typescriptreact = {
    create_file_header "//",
  },
  javascriptreact = {
    create_file_header "//",
  },
  -- Lua 文件头 (使用 -- 注释)
  lua = {
    create_file_header "--",
  },
  -- Go 文件头 (使用 // 注释)
  go = {
    create_file_header "//",
  },
  -- Java 文件头 (使用 // 注释)
  java = {
    create_file_header "//",
  },
  -- C# 文件头 (使用 // 注释)
  cs = {
    create_file_header "//",
  },
  -- HTML 文件头 (使用 <!-- 和 --> 注释)
  html = {
    s(
      "header",
      {
        t { "<!-- -----------------------------------------------------------------------------" },
        t { "    Copyright (C) " },
        f(get_current_year, {}),
        t { " mcge. All rights reserved." },
        t { "", " Author:         mcge" },
        t { "", " Email:          <mcgeq@outlook.com>" },
        t { "", " File:           " },
        f(get_filename, {}),
        t { "", " Description:    " },
        i(1, "Description of the file"),
        t { "", " Create   Date:  " },
        f(get_current_date_time, {}),
        t { "", " Last Modified:  " },
        f(get_current_date_time, {}),
        t { "", " Modified   By:  mcgeq <mcgeq@outlook.com>" },
        t { "", " ----------------------------------------------------------------------------- -->" },
        t { "" },
      }
    ),
  },
  -- CSS 文件头 (使用 /* 和 */ 注释)
  css = {
    s(
      "header",
      {
        t { "/* -----------------------------------------------------------------------------" },
        t { "    Copyright (C) " },
        f(get_current_year, {}),
        t { " mcge. All rights reserved." },
        t { "", " Author:         mcge" },
        t { "", " Email:          <mcgeq@outlook.com>" },
        t { "", " File:           " },
        f(get_filename, {}),
        t { "", " Description:    " },
        i(1, "Description of the file"),
        t { "", " Create   Date:  " },
        f(get_current_date_time, {}),
        t { "", " Last Modified:  " },
        f(get_current_date_time, {}),
        t { "", " Modified   By:  mcgeq <mcgeq@outlook.com>" },
        t { "", " ----------------------------------------------------------------------------- */" },
        t { "" },
      }
    ),
  },
  scss = {
    s(
      "header",
      {
        t { "/* -----------------------------------------------------------------------------" },
        t { "    Copyright (C) " },
        f(get_current_year, {}),
        t { " mcge. All rights reserved." },
        t { "", " Author:         mcge" },
        t { "", " Email:          <mcgeq@outlook.com>" },
        t { "", " File:           " },
        f(get_filename, {}),
        t { "", " Description:    " },
        i(1, "Description of the file"),
        t { "", " Create   Date:  " },
        f(get_current_date_time, {}),
        t { "", " Last Modified:  " },
        f(get_current_date_time, {}),
        t { "", " Modified   By:  mcgeq <mcgeq@outlook.com>" },
        t { "", " ----------------------------------------------------------------------------- */" },
        t { "" },
      }
    ),
  },
}

