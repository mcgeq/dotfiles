local M = {}

local DEFAULT_FILETYPES = {
  basedpyright = { "python" },
  bashls = { "sh", "bash", "zsh" },
  clangd = { "c", "cpp", "objc", "objcpp", "cuda" },
  cssls = { "css", "scss", "less" },
  gopls = { "go", "gomod", "gowork", "gotmpl" },
  jsonls = { "json", "jsonc" },
  lua_ls = { "lua" },
  marksman = { "markdown", "markdown.mdx" },
  ruff = { "python" },
  rust_analyzer = { "rust" },
  taplo = { "toml" },
  yamlls = { "yaml" },
  zls = { "zig", "zir" },
}

local function server_filetypes(name, entry)
  local config = entry.config or {}
  return config.filetypes or (vim.lsp.config[name] or {}).filetypes or DEFAULT_FILETYPES[name] or {}
end

local function build_filetype_server_index(registry)
  local index = {}

  for name, entry in pairs(registry) do
    if entry.ensure_installed and entry.enable ~= false then
      for _, filetype in ipairs(server_filetypes(name, entry)) do
        index[filetype] = index[filetype] or {}
        table.insert(index[filetype], name)
      end
    end
  end

  for _, names in pairs(index) do
    table.sort(names)
  end

  return index
end

local function mason_package_name(name)
  local ok, mappings = pcall(require, "mason-lspconfig.mappings")
  local names = ok and mappings.get_mason_map().lspconfig_to_package or {}
  return names[name] or name
end

local function retry_lsp_attach(name, bufnr)
  if not vim.api.nvim_buf_is_valid(bufnr) then return end
  vim.lsp.enable(name)
  pcall(vim.api.nvim_exec_autocmds, "FileType", {
    group = "nvim.lsp.enable",
    buffer = bufnr,
    modeline = false,
  })
end

local function setup_mason_auto_install(registry)
  if #vim.api.nvim_list_uis() == 0 then return end

  local filetype_servers = build_filetype_server_index(registry)
  local installing = {}
  local registry_ready = false
  local registry_refreshing = false
  local registry_callbacks = {}

  local function with_mason_registry(callback)
    local ok_registry, mason_registry = pcall(require, "mason-registry")
    if not ok_registry then return end

    if registry_ready then
      callback(mason_registry)
      return
    end

    table.insert(registry_callbacks, callback)
    if registry_refreshing then return end

    registry_refreshing = true
    mason_registry.refresh(function(success)
      registry_ready = success
      registry_refreshing = false

      local callbacks = registry_callbacks
      registry_callbacks = {}

      if not success then
        vim.schedule(
          function()
            vim.notify("Mason registry refresh failed; LSP auto-install skipped.", vim.log.levels.WARN, {
              title = "Mason",
            })
          end
        )
        return
      end

      for _, cb in ipairs(callbacks) do
        cb(mason_registry)
      end
    end)
  end

  local function ensure_server_installed(name)
    local package_name = mason_package_name(name)
    if installing[package_name] then return end

    with_mason_registry(function(mason_registry)
      local ok_package, package = pcall(mason_registry.get_package, package_name)
      if not ok_package then
        vim.schedule(
          function()
            vim.notify(("Mason package not found for `%s`: %s"):format(name, package_name), vim.log.levels.WARN, {
              title = "Mason",
            })
          end
        )
        return
      end

      if package:is_installed() then return end
      if package:is_installing() then return end

      installing[package_name] = true
      vim.schedule(
        function()
          vim.notify(("Installing `%s` for `%s`..."):format(package_name, name), vim.log.levels.INFO, {
            title = "Mason",
          })
        end
      )

      local ok_install, err = pcall(function()
        package:install({}, function(success, result)
          installing[package_name] = nil

          vim.schedule(function()
            if success then
              vim.notify(("Installed `%s`."):format(package_name), vim.log.levels.INFO, {
                title = "Mason",
              })
              for _, buffer in ipairs(vim.api.nvim_list_bufs()) do
                if
                  vim.api.nvim_buf_is_loaded(buffer)
                  and vim.list_contains(server_filetypes(name, registry[name]), vim.bo[buffer].filetype)
                then
                  retry_lsp_attach(name, buffer)
                end
              end
            else
              vim.notify(("Failed to install `%s`: %s"):format(package_name, result), vim.log.levels.ERROR, {
                title = "Mason",
              })
            end
          end)
        end)
      end)

      if not ok_install then
        installing[package_name] = nil
        vim.schedule(
          function()
            vim.notify(("Failed to start Mason install for `%s`: %s"):format(package_name, err), vim.log.levels.ERROR, {
              title = "Mason",
            })
          end
        )
      end
    end)
  end

  local function ensure_buffer_servers(bufnr)
    if not vim.api.nvim_buf_is_valid(bufnr) then return end

    local names = filetype_servers[vim.bo[bufnr].filetype]
    if not names then return end

    for _, name in ipairs(names) do
      if not registry[name].install_for or registry[name].install_for(bufnr) then ensure_server_installed(name) end
    end
  end

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("lsp_mason_auto_install", { clear = true }),
    callback = function(event) ensure_buffer_servers(event.buf) end,
  })

  vim.schedule(function()
    for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
      ensure_buffer_servers(bufnr)
    end
  end)
end

function M.setup()
  if vim.env.NVIM_NO_PLUGINS == "1" then return end
  local ok_mason, mason = pcall(require, "mason")
  if ok_mason then mason.setup {
    ui = {
      border = "rounded",
    },
  } end

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
    vim.list_extend(tools, require("user").lang.mason or {})
    tools = vim.fn.uniq(vim.fn.sort(tools))

    tool_installer.setup {
      ensure_installed = tools,
      run_on_start = false,
      auto_update = false,
    }
  end
  local ok, mason_lsp = pcall(require, "mason-lspconfig")
  if ok then mason_lsp.setup { ensure_installed = {}, automatic_enable = false } end
end

function M.setup_lsp(registry)
  if require("user").lsp.auto_install ~= false then setup_mason_auto_install(registry) end
end

return M
