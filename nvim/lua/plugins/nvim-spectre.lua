-- nvim-spectre 快捷键配置
-- AstroCommunity pack 不包含默认快捷键，需要手动添加

return {
  "nvim-pack/nvim-spectre",
  keys = {
    -- 全局搜索替换
    {
      "<leader>sr",
      function() require("spectre").toggle() end,
      desc = "Replace in files (Spectre)",
    },
    -- 搜索当前单词
    {
      "<leader>sw",
      function() require("spectre").open_visual({ select_word = true }) end,
      desc = "Search current word",
    },
    -- 在当前文件中搜索
    {
      "<leader>sp",
      function() require("spectre").open_file_search({ select_word = true }) end,
      desc = "Search in current file",
    },
  },
}
