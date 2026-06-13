$files = @(
  'd:\config\dotfiles\nvim\lua\plugins\conform.lua',
  'd:\config\dotfiles\nvim\lua\plugins\flash.nvim.lua',
  'd:\config\dotfiles\nvim\lua\plugins\noice-config.lua',
  'd:\config\dotfiles\nvim\lua\plugins\snacks.nvim.lua',
  'd:\config\dotfiles\nvim\lua\plugins\treesitter.lua',
  'd:\config\dotfiles\nvim\lua\plugins\ui-enhanced.lua',
  'd:\config\dotfiles\nvim\docs\colorful-winsep-nvim.md',
  'd:\config\dotfiles\nvim\docs\edgy-nvim.md',
  'd:\config\dotfiles\nvim\docs\harpoon.md',
  'd:\config\dotfiles\nvim\docs\mini-ai.md',
  'd:\config\dotfiles\nvim\docs\neorg-guide.md',
  'd:\config\dotfiles\nvim\docs\neorg-quickref.md',
  'd:\config\dotfiles\nvim\docs\neorg.md',
  'd:\config\dotfiles\nvim\docs\neotest.md',
  'd:\config\dotfiles\nvim\docs\nvim-spectre.md',
  'd:\config\dotfiles\nvim\docs\nvim-spider.md',
  'd:\config\dotfiles\nvim\docs\nvim-toggler.md',
  'd:\config\dotfiles\nvim\docs\project-nvim.md',
  'd:\config\dotfiles\nvim\docs\python-ty-configuration.md',
  'd:\config\dotfiles\nvim\docs\rainbow-delimiter-indent-blankline.md',
  'd:\config\dotfiles\nvim\docs\README.md',
  'd:\config\dotfiles\nvim\docs\snacks-picker-optimization.md',
  'd:\config\dotfiles\nvim\docs\toggleterm-nvim.md',
  'd:\config\dotfiles\nvim\docs\windows-nvim.md',
  'd:\config\dotfiles\nvim\ENHANCEMENT_GUIDE.md',
  'd:\config\dotfiles\nvim\lazy-lock.json',
  'd:\config\dotfiles\nvim\.preset',
  'd:\config\dotfiles\nvim\nvim.log',
  'd:\config\dotfiles\nvim\neovim.yml'
)

foreach ($file in $files) {
  if (Test-Path -LiteralPath $file) {
    try {
      Remove-Item -LiteralPath $file -Force
      Write-Host "Removed: $file"
    } catch {
      Write-Warning "Failed to remove: $file"
      Write-Warning $_
    }
  }
}
