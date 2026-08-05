local M = {}

local exec = require "lang.runner.exec"
local ui = require "lang.runner.ui"
local frontend_filetypes = require "lang.frontend_filetypes"

local languages = {
  rust = require "lang.runner.rust",
  zig = require "lang.runner.zig",
  go = require "lang.runner.go",
  python = require "lang.runner.python",
  frontend = require "lang.runner.frontend",
}

local last_run = nil

local function language_for(filetype)
  if vim.tbl_contains(frontend_filetypes.lsp_filetypes, filetype) then return languages.frontend end
  return languages[filetype]
end

local function run_action(bufnr, filetype, action)
  if action == "repeat_last" then
    if not last_run then
      vim.notify("No previous runner command to repeat yet.", vim.log.levels.INFO, { title = filetype:upper() })
      return false
    end
    exec.execute_result(last_run)
    return true
  end

  if not exec.ensure_file_is_saved(bufnr, filetype:upper()) then return false end

  local language = language_for(filetype)
  local result, err
  if language then
    result, err = language.command(bufnr, action)
  end
  if not result then
    vim.notify(err or ("No runner is configured for " .. filetype .. " -> " .. action), vim.log.levels.INFO, {
      title = filetype:upper(),
    })
    return false
  end

  if result.clean then
    exec.remove_files(result.paths or {}, result.title or filetype:upper())
    return true
  end

  last_run = result
  exec.execute_result(result)
  return true
end

function M.run(action, bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local filetype = vim.bo[bufnr].filetype

  if action == "repeat_last" then
    if not last_run then
      vim.notify("No previous runner command to repeat yet.", vim.log.levels.INFO, { title = "Runner" })
      return false
    end
    exec.execute_result(last_run)
    return true
  end

  if not ui.supports(filetype) then
    vim.notify(
      "Runner currently supports rust, zig, go, python, javascript, typescript, and vue buffers.",
      vim.log.levels.INFO,
      {
        title = "Runner",
      }
    )
    return false
  end

  return run_action(bufnr, filetype, action)
end

function M.show_keys(bufnr)
  bufnr = bufnr or vim.api.nvim_get_current_buf()
  local filetype = vim.bo[bufnr].filetype
  if not ui.supports(filetype) then
    vim.notify(
      "RunnerKeys currently supports rust, zig, go, python, javascript, typescript, and vue buffers.",
      vim.log.levels.INFO,
      {
        title = "RunnerKeys",
      }
    )
    return
  end
  ui.show_keys(bufnr, filetype)
end

function M.setup()
  local map = require("core.keymaps").map

  vim.api.nvim_create_user_command(
    "RunnerKeys",
    function() M.show_keys(vim.api.nvim_get_current_buf()) end,
    { desc = "Show backend runner keymaps" }
  )

  vim.api.nvim_create_user_command(
    "RunnerInfo",
    function() M.show_keys(vim.api.nvim_get_current_buf()) end,
    { desc = "Show runner actions for the current buffer" }
  )

  vim.api.nvim_create_user_command(
    "RunnerBuild",
    function() M.run("build", vim.api.nvim_get_current_buf()) end,
    { desc = "Build the current project or buffer target" }
  )

  vim.api.nvim_create_user_command(
    "RunnerRun",
    function() M.run("run", vim.api.nvim_get_current_buf()) end,
    { desc = "Run the current project or buffer target" }
  )

  vim.api.nvim_create_user_command(
    "RunnerTest",
    function() M.run("test", vim.api.nvim_get_current_buf()) end,
    { desc = "Run the current project or buffer target test target" }
  )

  vim.api.nvim_create_user_command(
    "RunnerTestFile",
    function() M.run("test_file", vim.api.nvim_get_current_buf()) end,
    { desc = "Run tests for the current file when supported" }
  )

  vim.api.nvim_create_user_command(
    "RunnerRepeat",
    function() M.run("repeat_last", vim.api.nvim_get_current_buf()) end,
    { desc = "Repeat the last runner command" }
  )

  vim.api.nvim_create_user_command(
    "RunnerClean",
    function() M.run("clean", vim.api.nvim_get_current_buf()) end,
    { desc = "Clean the current project or buffer target" }
  )

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("lang_runner_keymaps", { clear = true }),
    pattern = { "rust", "zig", "go", "python", "javascript", "javascriptreact", "typescript", "typescriptreact", "vue" },
    callback = function(event)
      local filetype = vim.bo[event.buf].filetype
      ui.register_which_key(event.buf, filetype)
      for _, entry in ipairs(ui.entries(filetype)) do
        if entry.key == "rk" then
          map(
            "n",
            "<localleader>rk",
            function() ui.show_keys(event.buf, filetype) end,
            entry.desc,
            { buffer = event.buf }
          )
        else
          map(
            "n",
            "<localleader>" .. entry.key,
            function() run_action(event.buf, filetype, entry.action) end,
            entry.desc,
            { buffer = event.buf }
          )
        end
      end
    end,
  })
end

return M
