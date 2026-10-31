-- Personal C/C++ preferences belong in user.lang.cpp; wiring lives in plugins.cpp.
return {
  feature = "cpp",
  lazy = true,
  spec = {
    { src = "https://github.com/p00f/clangd_extensions.nvim", name = "clangd_extensions.nvim" },
    { src = "https://github.com/Civitasv/cmake-tools.nvim", name = "cmake-tools.nvim" },
  },
  setup = function() require("plugins.cpp").setup() end,
}
