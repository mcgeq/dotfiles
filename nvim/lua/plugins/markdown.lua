local M = {}

function M.setup()
  local map = require("core.keymaps").map
  local loader = require("pack.loader")
  local markdown = require("lang.markdown")
  local extras_ready = false

  local function ensure_markdown_extras()
    if extras_ready then return true end
    if not loader.ensure_many({ "vim-table-mode", "img-clip.nvim", "vim-markdown-toc", "render-markdown.nvim" }) then
      return false
    end

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

    local ok_render_markdown, render_markdown = pcall(require, "render-markdown")
    if ok_render_markdown then
      render_markdown.setup(markdown.config().render or {})
    end

    extras_ready = true
    return true
  end

  markdown.setup()

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("plugins_markdown_keymaps", { clear = true }),
    pattern = "markdown",
    callback = function(event)
      if not ensure_markdown_extras() then return end
      local render_cfg = markdown.config().render or {}
      if render_cfg.enabled ~= false then
        local ok_render_markdown, render_markdown = pcall(require, "render-markdown")
        if ok_render_markdown then render_markdown.buf_enable() end
      end
      map("n", "<localleader>mf", "<cmd>MarkdownTableFormat<cr>", "Format markdown table", { buffer = event.buf })
      map("n", "<localleader>mr", "<cmd>RenderMarkdown buf_toggle<cr>", "Toggle markdown render", { buffer = event.buf })
      map("n", "<localleader>mp", "<cmd>RenderMarkdown preview<cr>", "Preview rendered markdown", { buffer = event.buf })
      map("n", "<localleader>mt", "<cmd>TableModeToggle<cr>", "Toggle table mode", { buffer = event.buf })
      map("n", "<localleader>mi", "<cmd>PasteImage<cr>", "Paste markdown image", { buffer = event.buf })
      map("n", "<localleader>mT", "<cmd>GenTocGFM<cr>", "Generate markdown TOC", { buffer = event.buf })
    end,
  })
end

return M
