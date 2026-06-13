# Jujutsu Configuration

Jujutsu (`jj`) version control system config with rich revset aliases, custom log templates, Meld difftool integration, and Gerrit support.

## Features

- **Editor**: Neovim
- **Diff/Merge**: Meld (graphical), difft (terminal), mergiraf (fast 3-way)
- **Git integration**: Colocated `.jj`/`.git`, subprocess git
- **Revset aliases**: `open()`, `ready()`, `wip()`, `private()`, `stack()`, `trunk()`, `user()`
- **Command aliases**: `s` (show), `d` (diff), `ll` (detailed log), `open`, `ready`, `retrunk`, `reheat`
- **Log templates**: `log1`, `log2`, `log3`, `logv`, `log1current`, `log1author`, `log1nobookmarks`
- **Colors**: Custom node/change-id/bookmark/tag colors
- **Gerrit**: Optional (disabled by default)

## Aliases

| Alias | Command | Description |
|-------|---------|-------------|
| `d` | `diff` | Show diff |
| `s` | `show` | Show commit details |
| `ll` | `log -T builtin_log_detailed` | Detailed log |
| `nt` | `new trunk()` | New change from trunk |
| `open` | `log -r open()` | Show open commits |
| `ready` | `log -r ready()` | Show ready commits |
| `retrunk` | `rebase -d trunk()` | Rebase to latest trunk |
| `reheat` | full rebase of stack | Rebase entire stack |
| `consume` | `squash --into @ --from` | Squash into current |
| `eject` | `squash --from @ --into` | Eject from current |

See [config.toml](config.toml) for full details.
