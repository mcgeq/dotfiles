local cfg = require("mcge").config
local mgAutoGroup = vim.api.nvim_create_augroup("mgAutoGroup", {
  clear = true,
})

local autocmd = vim.api.nvim_create_autocmd

if cfg.enable_imselect then
  autocmd("InsertLeave", {
    group = mgAutoGroup,
    callback = require("mcge.utils.im-select").insertLeave,
  })

  autocmd("InsertEnter", {
    group = mgAutoGroup,
    callback = require("mcge.utils.im-select").insertEnter,
  })
end

-- format on save
autocmd("BufWritePre", {
  group = mgAutoGroup,
  -- pattern = require("mcge.env").getFormatOnSavePattern(),
  callback = function()
    vim.lsp.buf.format()
  end,
})

-- set *.mdx to filetype to markdown
autocmd({ "BufNewFile", "BufRead" }, {
  group = mgAutoGroup,
  pattern = "*.mdx",
  command = "setfiletype markdown",
})

-- set wrap only in markdown
autocmd("FileType", {
  group = mgAutoGroup,
  pattern = { "markdown" },
  callback = function()
    if cfg.markdown then
      vim.opt_local.wrap = cfg.markdown.wrap
      vim.wo.wrap = cfg.markdown.wrap
    end
  end,
})

-- highlight on yank
autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = mgAutoGroup,
  pattern = "*",
})

-- https://www.reddit.com/r/neovim/comments/zc720y/tip_to_manage_hlsearch/
vim.on_key(function(char)
  if vim.fn.mode() == "n" then
    vim.opt.hlsearch = vim.tbl_contains({ "<CR>", "n", "N", "*", "#", "?", "/" }, vim.fn.keytrans(char))
  end
end, vim.api.nvim_create_namespace("auto_hlsearch"))

-- do not continue comments when type o
autocmd("BufEnter", {
  group = mgAutoGroup,
  pattern = "*",
  callback = function()
    vim.opt.formatoptions = vim.opt.formatoptions
      - "o" -- O and o, don't continue comments
      + "r" -- But do continue when pressing enter.
  end,
})

-- quit window with q
autocmd({ "FileType" }, {
  group = mgAutoGroup,
  pattern = {
    "help",
    "man",
    "neotest-output",
  },
  callback = function()
    keymap({ "i", "n" }, { "q", "<esc>" }, "<esc>:close<CR>", { buffer = true })
  end,
})

-- set indent size
autocmd({ "FileType" }, {
  callback = function()
    local ft = vim.bo.filetype
    local default_indent_size = 4
    local indent_size
    local switch = {
      javascript = cfg.frontend.indent or default_indent_size,
      javascriptreact = cfg.frontend.indent or default_indent_size,
      typescript = cfg.frontend.indent or default_indent_size,
      typescriptreact = cfg.frontend.indent or default_indent_size,
      c = cfg.clangd.indent or default_indent_size,
      cpp = cfg.clangd.indent or default_indent_size,
      go = cfg.golang.indent or default_indent_size,
      lua = cfg.lua.indent or default_indent_size,
      rust = cfg.rust.indent or default_indent_size,
      sh = cfg.bash.indent or default_indent_size,
      python = cfg.python.indent or default_indent_size,
      ruby = cfg.ruby.indent or default_indent_size,
      json = cfg.json.indent or default_indent_size,
      toml = cfg.toml.indent or default_indent_size,
      yaml = cfg.yaml.indent or default_indent_size,
      dockerfile = cfg.docker.indent or default_indent_size,
      solidity = cfg.solidity.indent or default_indent_size,
      java = cfg.java.indent or default_indent_size,
    }
    indent_size = switch[ft] or default_indent_size
    vim.bo.tabstop = indent_size
    vim.bo.softtabstop = indent_size
    vim.bo.shiftwidth = indent_size
  end,
})
