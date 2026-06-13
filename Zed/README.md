# Zed Editor Configuration

Configuration files for the Zed editor.

## Files

- `settings.json` — Main Zed configuration

## Highlights

- **Vim mode**: Enabled with relative line numbers, smartcase search
- **Clipboard**: Always uses system clipboard
- **Font**: UI 16px, Buffer 15px
- **Tab size**: 2
- **Autosave**: On focus change
- **Theme**: System mode (One Light / One Dark)
- **Cursor blink**: Enabled
- **Yank highlight**: 200ms duration

## Vim Mode Features

### Standard motions
- `h/j/k/l` — Cursor movement
- `g d` — Go to definition
- `g D` — Go to declaration
- `g y` — Go to type definition
- `g I` — Go to implementation
- `c d` — Rename
- `g A` — Find all references
- `g s` — Find symbol in file
- `g S` — Find symbol in project

For all available settings, run `zed: open default settings` from the command palette.
