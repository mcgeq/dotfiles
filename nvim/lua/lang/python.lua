local M = {}

function M.setup()
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "python",
    group = vim.api.nvim_create_augroup("lang_python_venv", { clear = true }),
    callback = function(event)
      if vim.b[event.buf].venv_checked then return end
      vim.b[event.buf].venv_checked = true

      local file = vim.api.nvim_buf_get_name(event.buf)
      local root = vim.fs.root(file, { "pyproject.toml", "uv.lock", "requirements.txt", ".git" })
      if not root then return end

      local is_windows = vim.fn.has("win32") == 1 or vim.fn.has("win64") == 1
      for _, name in ipairs({ ".venv", "venv" }) do
        local venv_dir = root .. (is_windows and "\\" or "/") .. name
        if vim.fn.isdirectory(venv_dir) == 1 then
          local python = is_windows and (venv_dir .. "\\Scripts\\python.exe") or (venv_dir .. "/bin/python")
          if vim.fn.filereadable(python) == 1 then
            vim.env.VIRTUAL_ENV = venv_dir
            vim.g.python3_host_prog = python
            vim.b[event.buf].python3_host_prog = python
            vim.notify("Python virtualenv detected: " .. name, vim.log.levels.INFO, { title = "Python" })
            return
          end
        end
      end
    end,
  })
end

return M
