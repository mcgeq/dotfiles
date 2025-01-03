return {
  --------------------- colorschemes ------------------------------------------
 
  -- molokai
  { 
    "tanvirtin/monokai.nvim",
    config = function()
      require("mcge.plugins.monokai")
    end,
  },

  -- tokyonight
  {
    "folke/tokyonight.nvim",
    config = function()
      require("mcge.plugins.tokyonight")
    end,
  },

  -- gruvbox
  { "rktjmp/lush.nvim" },
  { "ellisonleao/gruvbox.nvim" },

  -- nord
  { "shaunsingh/nord.nvim" },

  -- onedark
  { "ful1e5/onedark.nvim" },

  -- nightfox
  { "EdenEast/nightfox.nvim" },

  -- dracula
  { "Mofiqul/dracula.nvim" },

  -- kanagawa
  { "rebelot/kanagawa.nvim" },
}
