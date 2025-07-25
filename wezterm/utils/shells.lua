-- 定义一个函数检查 shell 是否可用
local function is_shell_available(shell)
    local env_path = os.getenv("PATH")
    if not env_path then
        return false
    end

    local paths = {}
    -- Windows ; Unix :
    if package.config:sub(1,1) == "\\" then -- Windows
        paths = env_path:split(";")
    else
        paths = env_path:split(":")
    end
    -- 检查每个路径下是否存在shell 的执行文件
    for _, path in ipairs(paths) do
        local full_path = path .. "/" .. shell
        if package.config:sub(1,1) == "\\" then -- Windows
            full_path = full_path .. ".exe"
        end
        if os.rename(full_path, full_path)then
            return true
        end
    end
    return false
end

-- split
-- 用于在 Lua 种拆分字符串
function string:split(delimiter)
    local result = {}
    local from = 1
    local delim_from, delim_to = string.find(self, delimiter, from)
    while delim_from do
        table.insert(result, string.sub(self, from, delim_from -1 ))
        from = delim_to + 1
        delim_from, delim_to = string.find(self, delimiter, from)
    end
    table.insert(result, string.sub(self, from))
    return result
end

local function get_shell_available(shells)
    -- 检查传入的shells
    if #shells == 0 then
        return "None"
    end

    for _, shell in ipairs(shells) do
        if is_shell_available(shell) then
            return shell
        end
    end
    return "None"
end

local shells = {"pwsh", "powershell", "bash", "nu", "fish", "zsh"}

local default_shell = get_shell_available(shells)

return default_shell
