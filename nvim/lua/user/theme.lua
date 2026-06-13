return {
  -- Preferred colorscheme. If nil, falls back to legacy `colorscheme` then "tokyonight".
  name = nil,
  -- Example:
  -- name = "kanagawa",

  -- Override per-theme setup without editing base config.
  overrides = {
    -- kanagawa = {
    --   theme = "dragon",
    --   transparent = true,
    -- },
  },

  -- Optional highlight overrides. Can be a table or function(theme_name).
  -- Example:
  -- highlights = {
  --   NormalFloat = { bg = "NONE" },
  --   CursorLineNr = { bold = true },
  -- },
  highlights = nil,
}
