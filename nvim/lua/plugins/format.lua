local M = {}

local DEFAULT_FORMATTERS_BY_FT = {
  c = { "clang_format" },
  cpp = { "clang_format" },
  css = { "prettier" },
  go = { "gofumpt", "goimports" },
  html = { "prettier" },
  javascript = { "prettier" },
  javascriptreact = { "prettier" },
  json = { "prettier" },
  jsonc = { "prettier" },
  lua = { "stylua" },
  markdown = { "prettier" },
  python = { "ruff_fix", "ruff_format" },
  rust = { "rustfmt" },
  scss = { "prettier" },
  sh = { "shfmt" },
  toml = { "taplo" },
  typescript = { "prettier" },
  typescriptreact = { "prettier" },
  vue = { "prettier" },
  yaml = { "prettier" },
  zig = { "zigfmt" },
}

local function normalize_newlines(text)
  return (text or ""):gsub("\r\n", "\n")
end

local function get_buffer_text(bufnr)
  local text = table.concat(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false), "\n")
  if vim.bo[bufnr].eol then text = text .. "\n" end
  return text
end

local function set_buffer_text(bufnr, text)
  local normalized = normalize_newlines(text)
  local has_eol = normalized:sub(-1) == "\n"
  if has_eol then normalized = normalized:sub(1, -2) end

  local lines = normalized == "" and { "" } or vim.split(normalized, "\n", { plain = true })
  local view = vim.fn.winsaveview()
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)
  vim.bo[bufnr].eol = has_eol
  vim.fn.winrestview(view)
end

local function ruff_stdin_command(args, stdin, opts)
  local ok_system, proc = pcall(vim.system, vim.list_extend({ "ruff" }, args), {
    cwd = opts.cwd,
    env = vim.fn.environ(),
    stdin = stdin,
    text = true,
  })
  if not ok_system then return nil, proc end

  local ok_wait, result = pcall(proc.wait, proc, opts.timeout_ms or 1000)
  if not ok_wait then return nil, result end
  return result
end

local function supports_python_ruff_fallback(formatters_by_ft)
  local python = formatters_by_ft.python
  if type(python) ~= "table" then return false end
  return #python == 2 and python[1] == "ruff_fix" and python[2] == "ruff_format"
end

local function format_python_with_ruff(bufnr, opts)
  opts = opts or {}
  if not vim.api.nvim_buf_is_valid(bufnr) or vim.bo[bufnr].filetype ~= "python" then return false end
  if vim.fn.executable("ruff") ~= 1 then return false end

  local filename = vim.api.nvim_buf_get_name(bufnr)
  if filename == "" then return false end

  local cwd = vim.fs.dirname(filename)
  local text = get_buffer_text(bufnr)
  local original = text
  local commands = {
    { "check", "--fix", "--quiet", "--stdin-filename", filename, "-" },
    { "format", "--stdin-filename", filename, "-" },
  }

  for _, args in ipairs(commands) do
    local result, err = ruff_stdin_command(args, text, {
      cwd = cwd,
      timeout_ms = opts.timeout_ms,
    })
    if not result then
      if opts.notify_errors then
        vim.notify("Ruff fallback failed: " .. tostring(err), vim.log.levels.WARN, { title = "Python Format" })
      end
      return false
    end

    local output = normalize_newlines(result.stdout or "")
    if output ~= "" then
      text = output
    elseif result.code ~= 0 then
      if opts.notify_errors then
        local stderr = normalize_newlines(result.stderr or ""):gsub("%s+", " ")
        if stderr == "" then stderr = "ruff exited with code " .. tostring(result.code) end
        vim.notify("Ruff fallback failed: " .. stderr, vim.log.levels.WARN, { title = "Python Format" })
      end
      return false
    end
  end

  if text ~= original then set_buffer_text(bufnr, text) end
  return true
end

function M.setup()
  local map = require("core.keymaps").map
  local user_lang = require("core.util").require_if_exists("user.lang") or {}
  local frontend_state = require("lang.frontend_state")

  local function current_frontend()
    return frontend_state.get()
  end

  local function frontend_owns_format(bufnr)
    local frontend = current_frontend()
    local eslint_filetypes = frontend_state.eslint_lookup()
    if frontend.formatter == "conform" or not eslint_filetypes[vim.bo[bufnr].filetype] then return false end
    if #vim.lsp.get_clients({ bufnr = bufnr, name = "eslint" }) > 0 then return true end
    return vim.fs.root(bufnr, {
      "eslint.config.js",
      "eslint.config.mjs",
      "eslint.config.cjs",
      "eslint.config.ts",
      ".eslintrc",
      ".eslintrc.js",
      ".eslintrc.cjs",
      ".eslintrc.json",
      ".eslintrc.yaml",
      ".eslintrc.yml",
    }) ~= nil
  end

  local formatters_by_ft = vim.tbl_deep_extend("force", vim.deepcopy(DEFAULT_FORMATTERS_BY_FT), user_lang.formatters_by_ft or {})
  local python_ruff_fallback = supports_python_ruff_fallback(formatters_by_ft)
  local ok_conform, conform = pcall(require, "conform")
  if ok_conform then
    local formatters = {
      ruff_fix = {
        command = "ruff",
        args = { "check", "--fix", "--quiet", "--stdin-filename", "$FILENAME", "-" },
        stdin = true,
      },
      ruff_format = {
        command = "ruff",
        args = { "format", "--stdin-filename", "$FILENAME", "-" },
        stdin = true,
      },
    }
    formatters = vim.tbl_deep_extend("force", formatters, user_lang.formatters or {})

    conform.setup({
      format_on_save = function(bufnr)
        if frontend_owns_format(bufnr) then return end
        return {
          timeout_ms = 1000,
          lsp_format = "fallback",
        }
      end,
      formatters_by_ft = formatters_by_ft,
      formatters = formatters,
    })
  elseif python_ruff_fallback then
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = vim.api.nvim_create_augroup("plugins_format_python_ruff_fallback", { clear = true }),
      pattern = "*.py",
      callback = function(event)
        if frontend_owns_format(event.buf) then return end
        format_python_with_ruff(event.buf, { timeout_ms = 1000 })
      end,
    })
  end

  map({ "n", "v" }, "<leader>cf", function()
    local bufnr = vim.api.nvim_get_current_buf()
    if frontend_owns_format(bufnr) and vim.fn.exists(":EslintFixAll") == 2 then
      pcall(vim.cmd, "silent EslintFixAll")
      return
    end

    if ok_conform then
      conform.format({ async = true })
      return
    end

    if python_ruff_fallback and format_python_with_ruff(bufnr, { notify_errors = true, timeout_ms = 1000 }) then return end
    vim.lsp.buf.format({ async = true })
  end, "Format buffer")

  local ok_mason, mason = pcall(require, "mason")
  if ok_mason then
    mason.setup({
      ui = {
        border = "rounded",
      },
    })
  end

  local ok_tool_installer, tool_installer = pcall(require, "mason-tool-installer")
  if ok_tool_installer then
    local tools = {
      "clang-format",
      "codelldb",
      "debugpy",
      "gofumpt",
      "goimports",
      "prettier",
      "ruff",
      "shfmt",
      "stylua",
      "taplo",
    }
    vim.list_extend(tools, user_lang.mason or {})
    tools = vim.fn.uniq(vim.fn.sort(tools))

    tool_installer.setup({
      ensure_installed = tools,
      run_on_start = false,
      auto_update = false,
    })
  end
end

return M
