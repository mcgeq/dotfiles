local platform = require("utils.platform")()
local default_shell = require("utils.shells")

local options = {
  default_prog = {},
  launch_menu = {},
}

if platform.is_win then
    if(default_shell == "None") then
        default_shell = "cmd"
    end
  options.default_prog = { default_shell }
  options.launch_menu = {
    { label = " PowerShell v1", args = { "powershell" } },
    { label = " PowerShell v7", args = { "pwsh" } },
    { label = " Cmd", args = { "cmd" } },
    { label = " Nushell", args = { "nu" } },
    {
      label = " GitBash",
      args = { "D:\\bin\\Git\\bin\\bash.exe" },
    }
  }
elseif platform.is_mac then
  options.default_prog = { "/opt/homebrew/bin/fish" }
  options.launch_menu = {
    { label = " Bash", args = { "bash" } },
    { label = " Fish", args = { "/opt/homebrew/bin/fish" } },
    { label = " Nushell", args = { "/opt/homebrew/bin/nu" } },
    { label = " Zsh", args = { "zsh" } },
  }
end

return options
