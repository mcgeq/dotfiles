return {
  "mrcjkb/rustaceanvim",
  opts = {
    server = {
      default_settings = {
        ["rust-analyzer"] = {
          procMacro = {
            enable = true,
            ignored = {},
          },
        },
      },
    },
  },
}
