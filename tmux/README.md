[中文](README.zh.md)

# tmux Configuration

Forked from [gpakosz/.tmux](https://github.com/gpakosz/.tmux). Customized status bar, keybindings, and defaults.

## Highlights

- **Prefix**: `C-b` (default)
- **History**: 5000 lines
- **Indexing**: Windows and panes start at 1
- **Mouse**: Enabled by default, toggle with `C-b m`
- **256-color**: `screen-256color` terminal type
- **Focus events**: On

## Custom Keybindings

### Pane management
| Binding | Action |
|---------|--------|
| `C-b s` | Split vertically |
| `C-b v` | Split horizontally |
| `C-b x` | Kill pane |
| `C-b +` | Maximize/restore pane |
| `C-b h/j/k/l` | Select pane (vim style) |
| `C-b H/J/K/L` | Resize pane by 2 cells |

### Window management
| Binding | Action |
|---------|--------|
| `C-b C-h` | Previous window |
| `C-b C-l` | Next window |
| `C-b Tab` | Last window |

### Other
| Binding | Action |
|---------|--------|
| `C-b r` | Reload config |
| `C-b m` | Toggle mouse |
| `C-b Enter` | Enter copy mode |
| `C-b b` | List buffers |
| `C-b p` | Paste buffer |
| `C-b P` | Choose buffer |
