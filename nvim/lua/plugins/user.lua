-- You can also add or configure plugins by creating files in this `plugins/` folder
-- PLEASE REMOVE THE EXAMPLES YOU HAVE NO INTEREST IN BEFORE ENABLING THIS FILE
-- Here are some examples:

---@type LazySpec
return {

  -- == Examples of Adding Plugins ==

  {
    "ray-x/lsp_signature.nvim",
    event = "BufRead",
    config = function() require("lsp_signature").setup() end,
  },

  -- == Examples of Overriding Plugins ==
  -- Note: snacks.nvim configuration is in lua/plugins/snacks.nvim.lua

  -- You can disable default plugins as follows:
  { "max397574/better-escape.nvim", enabled = false },

  -- You can also easily customize additional setup of plugins that is outside of the plugin's setup call
  {
    "L3MON4D3/LuaSnip",
    config = function(plugin, opts)
      require "astronvim.plugins.configs.luasnip"(plugin, opts) -- include the default astronvim config that calls the setup call
      -- add more custom luasnip configuration such as filetype extend or custom snippets
      local luasnip = require "luasnip"
      luasnip.filetype_extend("javascript", { "javascriptreact" })

      -- 加载自定义文件头 snippets
      local snippets = require "config.snippets"
      local ls = require "luasnip"

      -- 为每个文件类型加载 snippets
      for filetype, filetype_snippets in pairs(snippets) do
        ls.add_snippets(filetype, filetype_snippets)
      end
    end,
  },

  {
    "windwp/nvim-autopairs",
    config = function(plugin, opts)
      require "astronvim.plugins.configs.nvim-autopairs"(plugin, opts)
      -- Add custom autopairs rules if needed
      -- local npairs = require "nvim-autopairs"
      -- local Rule = require "nvim-autopairs.rule"
      -- npairs.add_rules({ Rule("$", "$", { "tex", "latex" }) })
    end,
  },
}
