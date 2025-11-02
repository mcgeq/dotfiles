-- lua/config/lsp_trouble_float.lua
---@module "config.lsp_trouble_float"
---Custom Trouble.nvim floating window configuration with adaptive sizing
local M = {}

---Get adaptive window configuration based on screen size
---@return table config Table containing window size and position settings
local function get_adaptive_config()
  local screen_width = vim.o.columns

  local config = {
    main_width = 0.45,
    main_height = 0.45,
    main_position = { 0.3, 0.5 },
    preview_width = 0.45,
    preview_height = 0.6,
    preview_position = { 0.65, 0.5 },
  }

  if screen_width < 100 then
    config.main_width = 0.8
    config.main_height = 0.4
    config.main_position = { 0.5, 0.3 }
    config.preview_width = 0.8
    config.preview_height = 0.45
    config.preview_position = { 0.5, 0.7 }
  elseif screen_width < 140 then
    config.main_width = 0.6
    config.main_height = 0.45
    config.main_position = { 0.25, 0.5 }
    config.preview_width = 0.6
    config.preview_height = 0.55
    config.preview_position = { 0.7, 0.5 }
  elseif screen_width < 180 then
    config.main_width = 0.5
    config.main_height = 0.5
    config.main_position = { 0.25, 0.5 }
    config.preview_width = 0.5
    config.preview_height = 0.6
    config.preview_position = { 0.7, 0.5 }
  else
    config.main_width = 0.4
    config.main_height = 0.45
    config.main_position = { 0.2, 0.5 }
    config.preview_width = 0.4
    config.preview_height = 0.6
    config.preview_position = { 0.65, 0.5 }
  end

  return config
end

---Get Trouble window configuration with common settings
---@param config table Adaptive window configuration from get_adaptive_config()
---@param mode string Trouble mode (e.g., "lsp_references", "lsp_implementations")
---@param title string Window title
---@param title_icon string|nil Optional icon for the title
---@return table trouble_config Complete Trouble window configuration
local function get_trouble_config(config, mode, title, title_icon)
  return {
    mode = mode,
    focus = true,
    follow = true,
    auto_close = false,
    win = {
      type = "float",
      relative = "editor",
      border = "rounded",
      title = title_icon and (title_icon .. " " .. title) or title,
      title_pos = "center",
      position = config.main_position,
      size = { width = config.main_width, height = config.main_height },
      zindex = 200,
      wo = {
        winblend = 5,
        winhighlight = "Normal:TroubleNormal,FloatBorder:TroubleBorder,Title:TroubleTitle",
      },
    },
    preview = {
      type = "float",
      relative = "editor",
      border = "rounded",
      title = "📝 Context Preview",
      title_pos = "center",
      position = config.preview_position,
      size = { width = config.preview_width, height = config.preview_height },
      zindex = 150,
      wo = {
        winblend = 5,
        winhighlight = "Normal:TroublePreview,FloatBorder:TroubleBorder",
        number = true,
        relativenumber = false,
        cursorline = true,
      },
    },
    format = "{file_icon} {filename}:{pos} │ {item.text}",
    group = true,
    padding = false,
    icons = {
      indent = {
        top = "│ ",
        middle = "├╴",
        last = "└╴",
        fold_open = " ",
        fold_closed = " ",
        ws = " ",
      },
      folder_closed = " ",
      folder_open = " ",
      kinds = {
        File = "󰈙 ",
        Function = "󰊕 ",
        Method = "󰆧 ",
        Variable = "󰀫 ",
        Class = "󰠱 ",
        Interface = " ",
        Module = " ",
        Property = "󰜢 ",
        Constant = "󰏿 ",
        Enum = " ",
        EnumMember = " ",
        Struct = "󰙅 ",
        Event = " ",
        Operator = "󰆕 ",
        TypeParameter = "󰊄 ",
      },
    },
  }
end

---Setup Trouble floating window mappings for AstroLSP
---@param opts table|nil AstroLSP options table
---@return table opts Modified options table with Trouble mappings
function M.setup(opts)
  opts = opts or {}
  opts.mappings = opts.mappings or {}
  opts.mappings.n = opts.mappings.n or {}

  -- Ensure trouble.nvim is available
  local ok, trouble = pcall(require, "trouble")
  if not ok then
    vim.notify("trouble.nvim is not available. Please ensure it's installed.", vim.log.levels.WARN)
    return opts
  end

  -- 设置 trouble 窗口自定义键映射
  local function setup_trouble_keymaps()
    vim.schedule(function()
        local trouble_win = nil
        for _, win in ipairs(vim.api.nvim_list_wins()) do
          local buf = vim.api.nvim_win_get_buf(win)
          local buf_name = vim.api.nvim_buf_get_name(buf)
          if buf_name:match "Trouble" then
            trouble_win = win
            break
          end
        end

        if trouble_win then
          vim.api.nvim_set_current_win(trouble_win)
          local buf = vim.api.nvim_win_get_buf(trouble_win)

          -- 清理原有映射
          pcall(vim.keymap.del, "n", "<CR>", { buffer = buf })
          pcall(vim.keymap.del, "n", "o", { buffer = buf })
          pcall(vim.keymap.del, "n", "<Tab>", { buffer = buf })
          pcall(vim.keymap.del, "n", "p", { buffer = buf })
          pcall(vim.keymap.del, "n", "<Esc>", { buffer = buf })

          -- <CR> 和 o：跳转并关闭 trouble
          local function jump_and_close()
            local current_line = vim.fn.line('.')
            if current_line and current_line > 0 then
              local ok_trouble, trouble_mod = pcall(require, "trouble")
              if not ok_trouble then
                return
              end
              local view = trouble_mod.get_view()
              if view and view.items and view.items[current_line] then
                local item = view.items[current_line]
                if item.filename and item.filename ~= "" then
                  -- Use vim.fn.fnameescape for safer path handling
                  local escaped_path = vim.fn.fnameescape(item.filename)
                  vim.cmd(("edit %s"):format(escaped_path))
                  if item.lnum and item.lnum > 0 then
                    vim.api.nvim_win_set_cursor(0, { item.lnum, item.col or 0 })
                  end
                  trouble_mod.close()
                end
              end
            end
          end

          vim.keymap.set("n", "<CR>", jump_and_close, { buffer = buf, desc = "Jump and close", nowait = true, remap = false })
          vim.keymap.set("n", "o", jump_and_close, { buffer = buf, desc = "Jump and close", nowait = true, remap = false })

          -- <Tab> 和 p：预览 ±3 行上下文
          local function preview_with_context()
            local current_line = vim.fn.line('.')
            if current_line and current_line > 0 then
              local ok_trouble, trouble_mod = pcall(require, "trouble")
              if not ok_trouble then
                return
              end
              local view = trouble_mod.get_view()
              if view and view.items and view.items[current_line] then
                local item = view.items[current_line]
                if item.filename and item.filename ~= "" then
                  local context_before = 3
                  local target_line = math.max(1, item.lnum or 1)
                  local start_line = math.max(1, target_line - context_before)

                  -- Use vim.fn.fnameescape for safer path handling
                  local escaped_path = vim.fn.fnameescape(item.filename)
                  vim.cmd(("pedit +%d %s"):format(target_line, escaped_path))

                  vim.schedule(function()
                    for _, win in ipairs(vim.api.nvim_list_wins()) do
                      local buf = vim.api.nvim_win_get_buf(win)
                      local buf_name = vim.api.nvim_buf_get_name(buf)
                      if buf_name == item.filename then
                        local win_info = vim.api.nvim_win_get_config(win)
                        if win_info.relative ~= "" then
                          vim.api.nvim_win_set_cursor(win, { target_line, item.col or 0 })
                          vim.api.nvim_win_call(win, function()
                            -- Use format strings for better safety and readability
                            vim.cmd(("normal! %dG"):format(start_line))
                            vim.cmd(("normal! %dG"):format(target_line))
                            vim.cmd("normal! zz")
                            if target_line - start_line < 3 then
                              vim.cmd(("normal! %dk"):format(math.min(3, target_line - 1)))
                            end
                          end)
                          break
                        end
                      end
                    end
                  end)
                end
              end
            end
          end

          vim.keymap.set("n", "<Tab>", preview_with_context, { buffer = buf, desc = "Preview with context (±3 lines)", nowait = true, remap = false })
          vim.keymap.set("n", "p", preview_with_context, { buffer = buf, desc = "Preview with context (±3 lines)", nowait = true, remap = false })

          -- <Esc> 和 q 关闭窗口
          local function close_trouble()
            local ok, trouble_mod = pcall(require, "trouble")
            if ok and trouble_mod then
              trouble_mod.close()
            end
          end

          vim.keymap.set("n", "<Esc>", close_trouble, { buffer = buf, desc = "Close", nowait = true, remap = false })
          vim.keymap.set("n", "q", close_trouble, { buffer = buf, desc = "Close", nowait = true, remap = false })
        end
      end)
    end)
  end

  ---Create Trouble floating window mapping helper function
  ---@param mode string Trouble mode
  ---@param title string Window title
  ---@param title_icon string|nil Optional icon
  ---@return table mapping Table with function and description
  local function create_trouble_mapping(mode, title, title_icon)
    return {
      function()
        local ok_trouble, trouble_mod = pcall(require, "trouble")
        if not ok_trouble then
          vim.notify("trouble.nvim is not available", vim.log.levels.ERROR)
          return
        end
        local config = get_adaptive_config()
        local trouble_config = get_trouble_config(config, mode, title, title_icon)
        trouble_mod.open(trouble_config)
        setup_trouble_keymaps()
      end,
      desc = (title_icon or "") .. " " .. title .. " (Float)",
    }
  end

  -- grr: References (浮动窗口)
  opts.mappings.n.grr = create_trouble_mapping("lsp_references", "References", "🔍")

  -- gri: Implementation (浮动窗口)
  opts.mappings.n.gri = create_trouble_mapping("lsp_implementations", "Implementations", "⚡")

  return opts
end

return M