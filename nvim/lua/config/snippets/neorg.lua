-- Neorg 专用 Snippets
-- 快速插入 Neorg 常用语法结构

local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node

local M = {}

-- 获取当前日期
local function get_date()
  return os.date("%Y-%m-%d")
end

-- 获取当前时间
local function get_time()
  return os.date("%H:%M")
end

-- Neorg snippets
M.snippets = {
  -- 标题快捷输入
  s("h1", { t("* "), i(1, "标题") }),
  s("h2", { t("** "), i(1, "标题") }),
  s("h3", { t("*** "), i(1, "标题") }),
  s("h4", { t("**** "), i(1, "标题") }),
  s("h5", { t("***** "), i(1, "标题") }),
  s("h6", { t("****** "), i(1, "标题") }),

  -- TODO 项
  s("todo", { t("- ( ) "), i(1, "待办事项") }),
  s("done", { t("- (x) "), i(1, "已完成") }),
  s("pending", { t("- (-) "), i(1, "进行中") }),
  s("cancel", { t("- (_) "), i(1, "已取消") }),
  s("urgent", { t("- (!) "), i(1, "紧急") }),

  -- 列表
  s("ul", { t("- "), i(1, "列表项") }),
  s("ol", { t("~ "), i(1, "列表项") }),

  -- 链接
  s("link", { t("{"), i(1, "url"), t("}["), i(2, "文字"), t("]") }),
  s("ilink", { t("{"), i(1, "* 标题"), t("}") }),

  -- 代码块
  s("code", {
    t("@code "),
    i(1, "语言"),
    t({ "", "" }),
    i(2, "代码"),
    t({ "", "@end" }),
  }),

  -- 引用
  s("quote", { t("> "), i(1, "引用内容") }),

  -- 元数据
  s("meta", {
    t({ "@document.meta", "" }),
    t("title: "),
    i(1, "标题"),
    t({ "", "description: " }),
    i(2, "描述"),
    t({ "", "authors: " }),
    i(3, "作者"),
    t({ "", "categories: [" }),
    i(4, "分类"),
    t({ "]", "created: " }),
    f(function()
      return get_date()
    end, {}),
    t({ "", "@end" }),
  }),

  -- 日期和时间
  s("date", {
    f(function()
      return get_date()
    end, {}),
  }),
  s("time", {
    f(function()
      return get_time()
    end, {}),
  }),
  s("now", {
    f(function()
      return get_date() .. " " .. get_time()
    end, {}),
  }),

  -- 表格
  s("table", {
    t({ "| " }),
    i(1, "列1"),
    t(" | "),
    i(2, "列2"),
    t(" | "),
    i(3, "列3"),
    t({ " |", "" }),
  }),

  -- 会议记录模板
  s("meeting", {
    t({ "@document.meta", "" }),
    t("title: 会议记录 - "),
    f(function()
      return get_date()
    end, {}),
    t({ "", "categories: [会议]", "@end", "", "* 会议信息", "  - 时间：" }),
    f(function()
      return get_date() .. " " .. get_time()
    end, {}),
    t({ "", "  - 参与人：" }),
    i(1, "参与人"),
    t({ "", "  - 地点：" }),
    i(2, "地点"),
    t({ "", "", "* 议题", "  ** " }),
    i(3, "议题1"),
    t({ "", "  - ( ) " }),
    i(4, "待办事项"),
    t({ "", "", "* 决定事项", "  - " }),
    i(5, "决定"),
  }),

  -- GTD 项目模板
  s("project", {
    t("* "),
    i(1, "项目名称"),
    t({ "", "  ** 目标", "    " }),
    i(2, "项目目标"),
    t({ "", "  ** 任务", "    - ( ) " }),
    i(3, "任务1"),
    t({ "", "    - ( ) " }),
    i(4, "任务2"),
    t({ "", "  ** 进度", "    - 开始时间：" }),
    f(function()
      return get_date()
    end, {}),
    t({ "", "    - 预计完成：" }),
    i(5, "日期"),
  }),
}

return M
