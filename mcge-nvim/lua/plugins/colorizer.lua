return {
  "norcalli/nvim-colorizer.lua",
  config = function()
    require("colorizer").setup({
      "css", -- 支持 CSS 文件
      "tsx", -- 支持 TSX 文件
      "javascript", -- 支持 JS 文件
      "typescript", -- 支持 TS 文件
    }, {
      mode = "background", -- 显示为背景色块
      css = true, -- 启用 CSS 颜色解析
      names = true, -- 支持颜色名称（如 "red"）
    })
  end,
}
