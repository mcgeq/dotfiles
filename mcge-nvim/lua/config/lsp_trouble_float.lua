-- lua/config/lsp_trouble_float.lua
local M = {}

function M.setup(opts)
  if not opts.mappings then opts.mappings = {} end
  if not opts.mappings.n then opts.mappings.n = {} end

  -- 根据屏幕大小调整窗口配置的辅助函数
  local function get_adaptive_config()
    local screen_width = vim.o.columns
    local screen_height = vim.o.lines

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

  -- 设置 trouble 窗口自定义键映射的辅助函数
  local function setup_trouble_keymaps()
    vim.schedule(function()
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
          vim.keymap.set("n", "<CR>", function()
            local current_line = vim.fn.line('.')
            if current_line and current_line > 0 then
              local trouble = require("trouble")
              local view = trouble.get_view()
              if view and view.items and view.items[current_line] then
                local item = view.items[current_line]
                if item.filename then
                  vim.cmd("edit " .. item.filename)
                  if item.lnum then
                    vim.api.nvim_win_set_cursor(0, { item.lnum, item.col or 0 })
                  end
                  trouble.close()
                end
              end
            end
          end, { buffer = buf, desc = "Jump and close", nowait = true, remap = false })

          vim.keymap.set("n", "o", function()
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<CR>", true, false, true), "n", false)
          end, { buffer = buf, desc = "Jump and close", nowait = true, remap = false })

          -- <Tab> 和 p：预览 ±3 行上下文
          vim.keymap.set("n", "<Tab>", function()
            local current_line = vim.fn.line('.')
            if current_line and current_line > 0 then
              local trouble = require("trouble")
              local view = trouble.get_view()
              if view and view.items and view.items[current_line] then
                local item = view.items[current_line]
                if item.filename then
                  local context_before = 3
                  local context_after = 3
                  local target_line = item.lnum or 1
                  local start_line = math.max(1, target_line - context_before)
                  local end_line = target_line + context_after

                  vim.cmd("pedit +" .. target_line .. " " .. item.filename)

                  vim.schedule(function()
                    for _, win in ipairs(vim.api.nvim_list_wins()) do
                      local buf = vim.api.nvim_win_get_buf(win)
                      local buf_name = vim.api.nvim_buf_get_name(buf)
                      if buf_name == item.filename then
                        local win_info = vim.api.nvim_win_get_config(win)
                        if win_info.relative ~= "" then
                          vim.api.nvim_win_set_cursor(win, { target_line, item.col or 0 })
                          vim.api.nvim_win_call(win, function()
                            vim.cmd("normal! " .. start_line .. "G")
                            vim.cmd("normal! " .. target_line .. "G")
                            vim.cmd("normal! zz")
                            if target_line - start_line < 3 then
                              vim.cmd("normal! " .. math.min(3, target_line - 1) .. "k")
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
          end, { buffer = buf, desc = "Preview with context (±3 lines)", nowait = true, remap = false })

          vim.keymap.set("n", "p", function()
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<Tab>", true, false, true), "n", false)
          end, { buffer = buf, desc = "Preview with context (±3 lines)", nowait = true, remap = false })

          -- <Esc> 和 q 关闭窗口
          vim.keymap.set("n", "<Esc>", function()
            require("trouble").close()
          end, { buffer = buf, desc = "Close", nowait = true, remap = false })

          vim.keymap.set("n", "q", function()
            require("trouble").close()
          end, { buffer = buf, desc = "Close", nowait = true, remap = false })
        end
      end)
    end)
  end

  -- grr: References (浮动窗口) - 自适应配置
  opts.mappings.n.grr = {
    function()
      local config = get_adaptive_config()

      require("trouble").open {
        mode = "lsp_references",
        focus = true,
        follow = true,
        auto_close = false,
        win = {
          type = "float",
          relative = "editor",
          border = "rounded",
          title = "🔍 References",
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
      setup_trouble_keymaps()
    end,
    desc = "🔍 References (Float)",
  }

  -- gri: Implementation (浮动窗口) - 自适应配置
  opts.mappings.n.gri = {
    function()
      local config = get_adaptive_config()

      require("trouble").open {
        mode = "lsp_implementations",
        focus = true,
        follow = true,
        auto_close = false,
        win = {
          type = "float",
          relative = "editor",
          border = "rounded",
          title = "⚡ Implementations",
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
      setup_trouble_keymaps()
    end,
    desc = "⚡ Implementations (Float)",
  }

  return opts
end

return M