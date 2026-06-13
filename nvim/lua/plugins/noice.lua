local M = {}

function M.setup()
  local ok_noice, noice = pcall(require, "noice")
  if not ok_noice then return end

  noice.setup({
    notify = {
      enabled = false,
    },
    lsp = {
      progress = { enabled = true },
      override = {
        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
        ["vim.lsp.util.stylize_markdown"] = true,
      },
    },
    presets = {
      bottom_search = true,
      command_palette = true,
      long_message_to_split = true,
      inc_rename = false,
      lsp_doc_border = false,
    },
  })
end

return M
