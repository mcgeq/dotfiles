-- Conform.nvim 配置 - 代码格式化工具
-- 文档: https://github.com/stevearc/conform.nvim

---@type LazySpec
return {
  "stevearc/conform.nvim",
  opts = {
    -- 为不同文件类型配置格式化工具
    formatters_by_ft = {
      javascript = { "biome" },
      javascriptreact = { "biome" },
      typescript = { "biome" },
      typescriptreact = { "biome" },
      vue = { "biome" },
      json = { "biome" },
      jsonc = { "biome" },
    },

    -- 自定义 Biome 格式化器配置
    formatters = {
      biome = {
        -- 添加 --unsafe 标志以启用不安全的代码修复
        args = {
          "check",
          "--write",
          "--unsafe",
          "--stdin-file-path",
          "$FILENAME",
        },
      },
    },

    -- 保存时自动格式化
    format_on_save = {
      -- 这些选项将传递给 conform.format()
      timeout_ms = 1000, -- 优化：减少超时时间，提升响应速度
      lsp_fallback = true,
    },
  },
}
