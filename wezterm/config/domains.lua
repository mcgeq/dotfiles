local constants = require("config.constants")

return {
  -- ref: https://wezfurlong.org/wezterm/config/lua/SshDomain.html
  ssh_domains = {},

  -- ref: https://wezfurlong.org/wezterm/multiplexing.html#unix-domains
  unix_domains = {},

  -- ref: https://wezfurlong.org/wezterm/config/lua/WslDomain.html
  wsl_domains = {
    {
      name = constants.WSL.DOMAIN_NAME,
      distribution = constants.WSL.DISTRIBUTION,
      username = constants.WSL.USERNAME,
      default_cwd = constants.WSL.DEFAULT_CWD,
      default_prog = constants.WSL.DEFAULT_PROG,
    },
  },
}
