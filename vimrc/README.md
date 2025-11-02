# Vim 9 Configuration

A modern Vim configuration written in **Vim9script** with comprehensive language support using CoC.nvim (Conquer of Completion) and traditional Vim plugins.

## 🌟 Features

### Language Support (via CoC.nvim)
Full LSP and language support for 30+ languages:
- **Systems**: C/C++ (clangd), Java, CMake
- **Web**: TypeScript/JavaScript, HTML, CSS, Vue, Volar
- **Dynamic**: Python (Pyright), Lua
- **Rust**: rust-analyzer
- **Functional**: Clojure
- **Data**: JSON, SQL, TOML, YAML, XML
- **Markup**: Markdown, TailwindCSS, UnoCSS
- **Shell**: Bash, PowerShell
- **Other**: Git, Prettier, ESLint

### Core Plugins
- **Completion**: CoC.nvim - Language Server Protocol
- **Fuzzy Finder**: FZF, vim-clap
- **File Explorer**: CoC Explorer, Vista
- **Git**: vim-gitgutter, Fugitive
- **Terminal**: Floaterm
- **Statusline**: Airline
- **UI**: Which-key, Startify
- **Snippets**: Ultisnips with comprehensive snippets library
- **Commenting**: NERDCommenter

### Editor Features
- Auto-completion with IntelliSense
- Git integration and highlighting
- Syntax highlighting and error diagnostics
- Code formatting (Prettier, clang-format)
- Multiple cursors support
- Project-wide symbol search
- Floating terminal
- Session management

## 📦 Installation

### Prerequisites
- Vim 9.0+ (compiled with `+python3`, `+ruby`, `+nodejs`, `+clipboard`)
- Node.js 14+ (for CoC.nvim)
- Universal Ctags (for Vista)
- Git
- (Optional) FZF for fuzzy finding

### Quick Install

#### Windows:
```powershell
# Install from dotfiles location
cd D:\config\dotfiles\vimrc

# Run install script
.\install.bat
```

The install script will:
1. Link `init.vim` to your Vim runtime path
2. Setup pack directory structure
3. Configure CoC.nvim
4. Install plugin dependencies

#### Linux/macOS:
```bash
# Navigate to vimrc directory
cd ~/dotfiles/vimrc

# Run install script
./install.sh
```

### First Launch

1. Start Vim:
   ```bash
   vim
   ```

2. Install CoC extensions:
   ```vim
   :CocInstall
   ```

3. **Important**: Update your main `vimrc` or `~/.vimrc` to source this configuration:

   **For Windows**:
   ```vim
   vim9script

   def SetPackPath(custom_dir: string)
       var packpath_exist = index(split(&packpath, ','), custom_dir) != -1
       if !packpath_exist
           execute 'set packpath+=' .. custom_dir
       endif
   enddef

   # Set your custom variables
   g:mcge_custom_project = "F:/2024/projects"
   g:mcge_custom_workspace = "F:/workspace"
   g:mcge_custom_fzf_dir = "D:/bin/fzf"
   g:mcge_custom_preview_bash = "D:/bin/Git/bin/bash.exe"
   g:mcge_customvimrcdir = "D:/config/dotfiles/vimrc"

   SetPackPath(g:mcge_customvimrcdir)
   execute 'source ' .. fnameescape(g:mcge_customvimrcdir .. '/init.vim')
   ```

   **For Unix**:
   ```vim
   vim9script

   def SetPackPath(custom_dir: string)
       var packpath_exist = index(split(&packpath, ','), custom_dir) != -1
       if !packpath_exist
           execute 'set packpath+=' .. custom_dir
       endif
   enddef

   g:mcge_customvimrcdir = "~/dotfiles/vimrc"
   g:mcge_custom_project = "~/projects"
   g:mcge_custom_workspace = "~/workspace"
   g:mcge_custom_fzf_dir = "~/.fzf"
   g:mcge_custom_preview_bash = "/usr/bin/bash"

   SetPackPath(g:mcge_customvimrcdir)
   execute 'source ' .. fnameescape(g:mcge_customvimrcdir .. '/init.vim')
   ```

## 🗂️ Configuration Structure

```
vimrc/
├── init.vim                    # Main configuration entry point
├── vimrc.txt                   # Template for main vimrc
├── install.bat / install.sh    # Installation scripts
├── config/
│   ├── core/                   # Core functionality
│   │   ├── mcge_startify.vim   # Dashboard
│   │   └── mcge_which_key.vim  # Key binding guide
│   ├── etc/                    # Plugin configurations
│   │   ├── mcge_base.vim       # Base Vim settings
│   │   ├── mcge_ui.vim         # UI customization
│   │   ├── mcge_coc.vim        # CoC.nvim config
│   │   ├── mcge_fzf.vim        # FZF configuration
│   │   ├── mcge_keymap.vim     # Key mappings
│   │   ├── mcge_airline.vim    # Statusline
│   │   ├── mcge_floaterm.vim   # Floating terminal
│   │   ├── mcge_vista.vim      # Tag viewer
│   │   └── ...
│   ├── coc-settings.json       # CoC configuration
│   └── autoload/
│       └── mcge_utils.vim      # Utility functions
├── colors/                     # Custom colorschemes
│   └── mcge_scheme.vim
├── pack/                       # Vim 8+ native package manager
│   └── mcge/
│       └── start/              # Auto-loaded plugins
│           ├── coc.nvim/
│           ├── vim-fzf/
│           ├── airline/
│           ├── vim-snippets/   # Comprehensive snippets
│           └── ...
└── snippets/                   # Custom snippets
    ├── python.snippets
    ├── rust.snippets
    ├── typescript.snippets
    └── typescriptreact.snippets
```

## ⌨️ Key Bindings

### Leader Key: `<Space>`

#### General
- `<Leader> h` - Toggle file tree (CoC Explorer)
- `<Leader> f f` - Find files (FZF)
- `<Leader> f g` - Grep files
- `<Leader> f b` - Buffers
- `<Leader> b d` - Delete buffer
- `<Leader> q` - Quit

#### Code Actions (CoC)
- `gd` - Go to definition
- `gy` - Go to type definition
- `gi` - Go to implementation
- `gr` - References
- `K` - Hover documentation
- `<Leader> r n` - Rename symbol
- `<Leader> a` - Code actions
- `<Leader> f` - Format file

#### Navigation
- `<Ctrl-hjkl>` - Navigate splits
- `<Ctrl-o>` - Jump backward
- `<Ctrl-i>` - Jump forward
- `<Leader> t` - Floating terminal

#### Git
- `]c` - Next hunk
- `[c` - Previous hunk
- `<Leader> hs` - Stage hunk
- `<Leader> hr` - Reset hunk

#### Vista (Tags Viewer)
- `<Leader> v t` - Toggle Vista
- `<Leader> v f` - Find symbol in file
- `<Leader> v p` - Open project tree

#### Which Key
Press `<Leader>` and wait to see available key combinations.

## ⚙️ Configuration

### Environment Variables

Configure in your main `vimrc`:

```vim
# Windows
g:mcge_custom_project = "F:/2024/projects"
g:mcge_custom_workspace = "D:/workspaces"
g:mcge_custom_fzf_dir = "D:/bin/fzf"

# Unix
g:mcge_custom_project = "~/projects"
g:mcge_custom_workspace = "~/workspace"
g:mcge_custom_fzf_dir = "~/.fzf"
```

### CoC Configuration

Edit `config/coc-settings.json` to customize LSP behavior:

```json
{
  "python.linting.enabled": true,
  "python.formatting.provider": "black",
  "clangd.path": "/usr/bin/clangd",
  "rust-analyzer.checkOnSave.command": "clippy"
}
```

### Adding Snippets

Custom snippets are in `snippets/`. Format:

```snip
# python.snippets
snippet ifmain
abbr if __name__ == "__main__":
if __name__ == "__main__":
    ${1:main()}
```

### Auto-Update Timestamps

Automatically updates file headers when saving `.rs`, `.c`, `.cpp`, `.py`, `.ts`, `.cs` files:

```vim
autocmd BufWritePre *.{rs,c,cpp,py,ts,cs} mcge_utils.AutoUpdateLastUpdateInfo()
```

## 🔧 CoC Extensions Management

### Install Extension
```vim
:CocInstall coc-tsserver
```

### List Extensions
```vim
:CocList extensions
```

### Update Extensions
```vim
:CocUpdate
```

### Extension Status
```vim
:CocList services
```

## 📝 Snippets

### Built-in Snippets
Via `ultisnips` with comprehensive library:
- 100+ languages supported
- Thousands of snippets

### Custom Snippets
- `snippets/python.snippets` - Python templates
- `snippets/rust.snippets` - Rust boilerplate
- `snippets/typescript.snippets` - TypeScript/JavaScript
- `snippets/typescriptreact.snippets` - React/JSX

## 🎨 UI Customization

### Colorscheme
Custom colorscheme: `colors/mcge_scheme.vim`

Change colorscheme in `config/etc/mcge_ui.vim`:
```vim
colorscheme mcge_scheme
```

### Airline Theme
Configure in `config/etc/mcge_airline.vim`:
```vim
g:airline_theme = 'powerlineish'
```

### Font
Set in `config/etc/mcge_base.vim`:
```vim
set guifont=Consolas:h12
```

## 🔍 Troubleshooting

### CoC not working
1. Ensure Node.js is installed: `node --version`
2. Check CoC status: `:CocInfo`
3. Reinstall CoC: `:CocUninstall coc.nvim` then `:CocInstall coc.nvim`

### FZF not finding files
1. Ensure FZF is in PATH
2. Check `g:mcge_custom_fzf_dir` is correct
3. Verify executable: `fzf --version`

### Snippets not expanding
1. Check Ultisnips is loaded: `:echo g:did_plug`
2. Verify trigger key: `let g:UltiSnipsExpandTrigger='<Tab>'`

### Floaterm not opening
Ensure Node.js and Python 3 are installed and in PATH.

## 📚 Plugin Management

This configuration uses Vim 8+ native package management (`pack/`).

### Adding Plugins

1. Create plugin directory:
   ```bash
   mkdir -p pack/mcge/start/plugin-name
   ```

2. Install via git:
   ```bash
   cd pack/mcge/start/plugin-name
   git clone <repository-url> .
   ```

Or use [vim-plug](https://github.com/junegunn/vim-plug) for better management.

## 🆚 Vim9script Features

This configuration leverages Vim 9's modern features:
- Type safety
- Improved performance
- Modern syntax
- Better scoping

Example from the config:
```vim
vim9script

def SetPackPath(custom_dir: string)
    var packpath_exist = index(split(&packpath, ','), custom_dir) != -1
    if !packpath_exist
        execute 'set packpath+=' .. custom_dir
    endif
enddef
```

## 🤝 Contributing

This is a personal configuration. Feel free to:
1. Fork it for your own use
2. Adapt it to your workflow
3. Report issues or suggestions

## 📄 License

Same as the parent dotfiles repository.

## 🙏 Acknowledgments

- CoC.nvim team for amazing LSP integration
- All plugin authors for their contributions
- Vim community for constant improvements

## 📖 Resources

- [Vim 9 Documentation](https://vimhelp.org/if_vim9.txt.html)
- [CoC.nvim Documentation](https://github.com/neoclide/coc.nvim)
- [Ultisnips Documentation](https://github.com/SirVer/ultisnips)
- [FZF Documentation](https://github.com/junegunn/fzf)

