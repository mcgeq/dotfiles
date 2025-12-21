-- Python 虚拟环境自动检测
-- 简单可靠的解决方案，不依赖外部插件

---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      autocmds = {
        -- Python 虚拟环境自动检测
        python_venv_auto = {
          {
            event = "FileType",
            pattern = "python",
            desc = "Auto-detect and activate Python virtual environment",
            callback = function()
              -- 避免重复检测
              if vim.b.venv_checked then
                return
              end
              vim.b.venv_checked = true
              
              local cwd = vim.fn.getcwd()
              local is_windows = vim.fn.has("win32") == 1 or vim.fn.has("win64") == 1
              
              -- 检测顺序：.venv > venv
              local venv_names = { ".venv", "venv" }
              
              for _, venv_name in ipairs(venv_names) do
                local venv_path = cwd .. (is_windows and "\\" or "/") .. venv_name
                
                if vim.fn.isdirectory(venv_path) == 1 then
                  local python_path = is_windows
                    and venv_path .. "\\Scripts\\python.exe"
                    or venv_path .. "/bin/python"
                  
                  if vim.fn.filereadable(python_path) == 1 then
                    -- 设置 Python 路径
                    vim.g.python3_host_prog = python_path
                    
                    -- 保存完整路径到另一个变量（供 LSP 使用）
                    vim.env.VIRTUAL_ENV_FULL = venv_path
                    
                    -- 配置虚拟环境显示格式（可自定义）
                    -- 选项 1: " .venv"  (Python 图标)
                    -- 选项 2: "🐍 .venv"  (Emoji)
                    -- 选项 3: "[.venv]"  (方括号) ✓ 当前
                    local venv_display = "[" .. venv_name .. "]"  -- 方括号
                    
                    -- 设置简短的虚拟环境名称到 VIRTUAL_ENV（供状态栏显示）
                    vim.env.VIRTUAL_ENV = venv_display
                    
                    -- 设置 PATH
                    vim.env.PATH = (is_windows 
                      and venv_path .. "\\Scripts;" 
                      or venv_path .. "/bin:") .. vim.env.PATH
                    
                    -- 在状态栏显示简短名称
                    vim.b.venv_name = venv_name
                    vim.g.venv_display_name = venv_name
                    
                    return
                  end
                end
              end
            end,
          },
        },
      },
    },
  },
}
