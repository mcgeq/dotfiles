return {
  "echasnovski/mini.files",
  version = false,
  config = function(_, opts)
    -- 合并原始配置（如果有）与新配置
    opts = opts or {}
    opts.windows = opts.windows or {}
    opts.windows.preview = true
    opts.windows.width_preview = 60 -- 默认宽度
    opts.windows.width_focus = 50
    opts.hooks = {
      post_open = function()
        -- 动态调整预览窗口宽度为总宽度的 30%
        local win_id = require("mini.files").get_explorer_state().windows.preview
        if win_id then
          local total_width = vim.o.columns
          local new_width = math.floor(total_width * 0.3)
          vim.api.nvim_win_set_width(win_id, math.max(40, math.min(new_width, 80)))
        end
      end,
    }
    opts.options = opts.options or {}
    opts.options.use_as_default_explorer = false
    require("mini.files").setup(opts)
  end,
}
