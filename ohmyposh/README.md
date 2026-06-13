# oh-my-posh Configuration

Custom prompt theme for PowerShell with diamond-shaped block layout.

## File

- `mcgeq.omp.toml` — Prompt theme definition (version 3)

## Layout (3 blocks)

1. **Left prompt** (newline): OS icon → path → git status → jujutsu status
2. **Right prompt**: System info (RAM) → Node.js → PHP → npm → execution time → clock
3. **Left prompt end** (newline): `╰─` prefix for command input

## Features

- OS detection with Nerd Font icons (Arch, Windows, WSL, etc.)
- Git branch/stash/change indicators with color-coded status
- Jujutsu change ID display
- Execution time tracking (threshold: 150ms)
- Real-time clock
- Package manager version display (Node, PHP, npm)

See [mcgeq.omp.toml](mcgeq.omp.toml) for full theme definition.
