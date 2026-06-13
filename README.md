# Dotfiles

Personal cross-platform configuration files for development environment. Windows-centric, with cross-shell and cross-editor coverage.

## Overview

| Category | Component | Description | Docs |
|----------|-----------|-------------|------|
| **Shell** | [PowerShell](powershell/) | Module-based `$PROFILE`, prompt, aliases, PSReadLine | [README](powershell/README.md) |
| | [Nushell](nushell/) | (Deprecated — pwsh is now primary) | [README](nushell/README.md) |
| | [Fish](fish/) | Minimal cross-platform fallback | — |
| **Terminal** | [WezTerm](wezterm/) | Modular wezterm config with tab bar, keybindings, themes | [README](wezterm/README.md) |
| | [Alacritty](alacritty/) | GPU-accelerated terminal config | — |
| | [tmux](tmux/) | Terminal multiplexer config | — |
| **Prompt** | [Starship](starship/) | Cross-shell prompt for pwsh, nushell, fish | [README](starship/README.md) |
| | [oh-my-posh](ohmyposh/) | Alternative prompt theme | — |
| **Editor** | [Neovim](nvim/) | Neovim 0.12+ config with `vim.pack`, snacks, blink.cmp | [README](nvim/README.md) |
| | [Vim](vimrc/) | Legacy vimrc with pack plugins | [README](vimrc/README.md) |
| | [Emacs](emacs/) | Emacs config with lsp-bridge, org-mode | [README](emacs/README.org) |
| | [Zed](Zed/) | Zed editor settings, snippets, keymaps | [README](Zed/README.md) |
| **Git** | [Gitconfig](gitconfig/) | Global git config (user, aliases, diff, delta) | [README](gitconfig/README.md) |
| | [Jujutsu](jj/) | `jj` version control config | — |
| | [Lazygit](lazygit/) | Terminal UI for git | — |
| **Package** | [Scoop](scoop/) | Windows package manager bucket/app config | [README](scoop/README.md) |

### Automation & Tools

| Component | Description |
|-----------|-------------|
| [AutoKey](autokey/) | Linux desktop automation (hotstrings, macros) |
| [lf](lf/) | Terminal file manager config |
| [commit/](commit/) | Commitizen + commitlint + husky for standardized commits |
| [init.ps1](init.ps1) | Bootstrap script — create symlinks for all config components |
| [deinit.ps1](deinit.ps1) | Teardown script — remove symlinks created by init.ps1 |
| [install.bat](install.bat) | Legacy install entry point |

## Quick Start

```powershell
# Clone to a central location (e.g. ~/dotfiles or D:\config\dotfiles)
git clone <repo-url> D:\config\dotfiles

# Bootstrap all config symlinks
.\init.ps1 -All
```

The bootstrap script reads `.gitmodules` and `.gitattributes` to detect components and creates symlinks at their expected locations (e.g. `$env:USERPROFILE\.config\wezterm` → `wezterm\`).

## Structure

```
dotfiles/
├── init.ps1             # Bootstrap (create symlinks)
├── deinit.ps1           # Teardown (remove symlinks)
├── install.bat          # Legacy installer
├── .gitmodules          # Plugin submodules
├── alacritty/           # Alacritty terminal
├── autokey/             # AutoKey (Linux)
├── commit/              # Commitizen config
├── emacs/               # Emacs (site-lisp + config-org)
├── fish/                # Fish shell
├── gitconfig/           # Global .gitconfig
├── jj/                  # Jujutsu VCS
├── lazygit/             # Lazygit TUI
├── lf/                  # lf file manager
├── nushell/             # Nushell (deprecated)
├── nvim/                # Neovim 0.12+
├── ohmyposh/            # oh-my-posh prompt
├── powershell/          # PowerShell $PROFILE
├── scoop/               # Scoop config
├── starship/            # Starship prompt
├── tmux/                # tmux (cross-platform)
├── vimrc/               # Legacy Vim
├── wezterm/             # WezTerm
└── Zed/                 # Zed editor
```

## Platform Notes

- **Primary OS**: Windows (PowerShell 7.5+)
- **Cross-platform components**: WezTerm, Neovim, Vim, Emacs, Starship, Git, tmux, fish
- **Linux-only**: AutoKey
- **macOS/Linux fallback**: Fish shell, tmux
