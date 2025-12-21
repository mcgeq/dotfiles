-- Noice.nvim 配置覆盖
-- 修复 vim.notify 冲突问题

---@type LazySpec
return {
  -- 配置 nvim-notify 优先级
  {
    "rcarriga/nvim-notify",
    priority = 1000, -- 提高优先级，确保先加载
    config = function(_, opts)
      local notify = require("notify")
      notify.setup(opts)
      -- 设置为默认通知处理器
      vim.notify = notify
    end,
  },
  
  -- 配置 Noice - 禁用 notify 接管
  {
    "folke/noice.nvim",
    optional = true,
    priority = 999, -- 比 notify 稍低，确保在 notify 之后加载
    opts = function(_, opts)
      -- 确保基础配置存在
      opts = opts or {}
      
      -- 关键修复：禁用 noice 对 vim.notify 的接管
      if not opts.notify then
        opts.notify = {}
      end
      opts.notify.enabled = false -- 禁用 noice 的 notify 功能
      
      -- 配置路由规则
      opts.routes = opts.routes or {}
      
      -- 过滤掉 VenvSelect 的警告通知
      table.insert(opts.routes, {
        filter = {
          event = "notify",
          find = "VenvSelect",
        },
        opts = { skip = true }, -- 跳过 VenvSelect 的通知
      })
      
      -- 确保 LSP 配置正确
      opts.lsp = opts.lsp or {}
      opts.lsp.progress = opts.lsp.progress or { enabled = true }
      opts.lsp.override = opts.lsp.override or {}
      opts.lsp.override["vim.lsp.util.convert_input_to_markdown_lines"] = true
      opts.lsp.override["vim.lsp.util.stylize_markdown"] = true
      opts.lsp.override["cmp.entry.get_documentation"] = true
      
      -- 配置预设
      opts.presets = opts.presets or {}
      opts.presets.bottom_search = true
      opts.presets.command_palette = true
      opts.presets.long_message_to_split = true
      opts.presets.inc_rename = false
      opts.presets.lsp_doc_border = false
      
      return opts
    end,
  },
}
