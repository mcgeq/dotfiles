local M = {}

local function is_package_json(bufnr)
  local name = vim.api.nvim_buf_get_name(bufnr)
  return vim.fs.basename(name) == "package.json"
end

function M.setup()
  local map = require("core.keymaps").map

  local ok_package_info, package_info = pcall(require, "package-info")
  if ok_package_info then
    package_info.setup({
      autostart = true,
      hide_up_to_date = false,
      hide_unstable_versions = false,
    })

    vim.api.nvim_create_autocmd({ "BufReadPost", "BufNewFile" }, {
      group = vim.api.nvim_create_augroup("plugins_package_info_keymaps", { clear = true }),
      callback = function(event)
        if not is_package_json(event.buf) then return end

        map("n", "<localleader>ns", package_info.show, "Show package versions", { buffer = event.buf })
        map("n", "<localleader>nh", package_info.hide, "Hide package versions", { buffer = event.buf })
        map("n", "<localleader>nt", package_info.toggle, "Toggle package versions", { buffer = event.buf })
        map("n", "<localleader>nu", package_info.update, "Update package", { buffer = event.buf })
        map("n", "<localleader>nd", package_info.delete, "Delete package", { buffer = event.buf })
        map("n", "<localleader>ni", package_info.install, "Install package", { buffer = event.buf })
        map("n", "<localleader>nc", package_info.change_version, "Change package version", { buffer = event.buf })
      end,
    })
  end

  local ok_hurl, hurl = pcall(require, "hurl")
  if ok_hurl then
    hurl.setup({
      debug = false,
      show_notification = false,
      mode = "split",
      formatters = {
        json = { "jq" },
      },
    })

    vim.api.nvim_create_autocmd("FileType", {
      group = vim.api.nvim_create_augroup("plugins_hurl_keymaps", { clear = true }),
      pattern = "hurl",
      callback = function(event)
        map("n", "<localleader>ha", "<cmd>HurlRunnerAt<cr>", "Run request", { buffer = event.buf })
        map("n", "<localleader>hA", "<cmd>HurlRunner<cr>", "Run all requests", { buffer = event.buf })
        map("n", "<localleader>hm", "<cmd>HurlToggleMode<cr>", "Toggle Hurl mode", { buffer = event.buf })
        map("n", "<localleader>hv", "<cmd>HurlVerbose<cr>", "Run verbose request", { buffer = event.buf })
        map("v", "<localleader>hh", ":HurlRunner<cr>", "Run selected request", { buffer = event.buf })
      end,
    })
  end
end

return M
