-- Neorg 配置
-- 强大的笔记和组织工具
-- 文档: https://github.com/nvim-neorg/neorg

---@type LazySpec
return {
  "nvim-neorg/neorg",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "nvim-lua/plenary.nvim",
  },
  ft = "norg", -- 只在打开 .norg 文件时加载
  cmd = "Neorg", -- 或使用命令时加载
  opts = function(_, opts)
    -- 确保加载必要的模块
    opts.load = opts.load or {}
    
    -- 合并默认配置
    opts.load = vim.tbl_deep_extend("force", opts.load, {
      -- 核心功能
      ["core.defaults"] = {},
      ["core.concealer"] = {
        config = {
          icon_preset = "basic", -- 使用基础图标集
        },
      },
      ["core.dirman"] = {
        config = {
          workspaces = {
            notes = "~/notes", -- 默认笔记目录，可根据需要修改
          },
          default_workspace = "notes",
        },
      },
      -- 补全支持
      ["core.completion"] = {
        config = {
          engine = "nvim-cmp",
        },
      },
      -- 导出功能
      ["core.export"] = {},
      -- 日记功能
      ["core.journal"] = {
        config = {
          workspace = "notes",
        },
      },
    })
    
    return opts
  end,
}
