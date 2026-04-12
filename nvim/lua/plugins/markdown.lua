local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local markdown = require("lang.markdown")

  vim.g.table_mode_corner = "|"
  vim.g.table_mode_corner_corner = "|"
  vim.g.table_mode_header_fillchar = "-"

  local ok_imgclip, imgclip = pcall(require, "img-clip")
  if ok_imgclip then
    imgclip.setup({
      default = {
        dir_path = "assets/images",
        file_name = "%Y-%m-%d-%H-%M-%S",
        relative_to_current_file = true,
        use_absolute_path = false,
      },
    })
  end

  local ok_headlines, headlines = pcall(require, "headlines")
  if ok_headlines then
    headlines.setup({
      markdown = {
        fat_headlines = false,
      },
    })
  end

  markdown.setup()

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("plugins_markdown_keymaps", { clear = true }),
    pattern = "markdown",
    callback = function(event)
      map("n", "<localleader>mf", "<cmd>MarkdownTableFormat<cr>", "Format markdown table", { buffer = event.buf })
      map("n", "<localleader>mt", "<cmd>TableModeToggle<cr>", "Toggle table mode", { buffer = event.buf })
      map("n", "<localleader>mi", "<cmd>PasteImage<cr>", "Paste markdown image", { buffer = event.buf })
      map("n", "<localleader>mT", "<cmd>GenTocGFM<cr>", "Generate markdown TOC", { buffer = event.buf })
    end,
  })
end

return M
