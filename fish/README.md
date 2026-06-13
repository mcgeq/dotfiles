# Fish Shell Configuration

Minimal fish config as a cross-platform fallback shell, primarily used on Arch Linux (WSL).

## File

- `config.fish` — Interactive shell configuration

## Features

- **Emacs**: `e` → `emacs -nw`, path includes `~/emacs/bin`
- **Arch package management**: `pry` (paru update), `prs` (search), `pri` (install), `prr` (remove)
- **Git abbreviations**: `gad`, `gpl`, `gph`, `gst`, `gsi`, `gsh`, `gsm`
- **Cargo**: `~/.cargo/bin` in PATH
- **Starship**: Prompt initialized
- **pnpm**: `PNPM_HOME` in PATH
- **tmux**: Auto-start tmux session `main` on login (if not already in tmux)

See [config.fish](config.fish) for full details.
