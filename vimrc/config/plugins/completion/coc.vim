vim9script

# -----------------------------------------------------------------------------
# 基础路径 & 扩展
# -----------------------------------------------------------------------------
g:coc_config_home = g:mcge_customvimrcdir .. "/config"
g:coc_data_home = g:mcge_customvimrcdir .. "/coc-data"

g:coc_global_extensions = [
    "coc-clojure", 
    "coc-css",
    "coc-cssmodules",
    "coc-clangd",
    "coc-clang-format-style-options",
    "coc-explorer",
    "coc-eslint",
    "coc-git",
    "coc-highlight",
    "coc-html",
    "coc-json", 
    "coc-java",
    "coc-lua",
    "coc-lightbulb",
    "coc-lists",
    "coc-cmake",
    "coc-markdownlint",
    "coc-markdown-preview-enhanced",
    "coc-pairs",
    "coc-prettier",
    "coc-pyright",
    "coc-rust-analyzer",
    "coc-sh",
    "coc-sql",
    "coc-snippets",
    "coc-toml",
    "coc-tabnine",
    "coc-tsserver",
    "@yaegassy/coc-tailwindcss3",
    "coc-unocss",
    "coc-vetur",
    "@yaegassy/coc-volar",
    "coc-vimlsp",
    "coc-webview",
    "coc-xml",
    "coc-yaml",
    "coc-zig",
    "coc-yank",
]

# https://raw.githubusercontent.com/neoclide/coc.nvim/master/doc/coc-example-config.vim

# -----------------------------------------------------------------------------
# 行为配置（autocmd）
# -----------------------------------------------------------------------------
augroup mcge_coc_core
  autocmd!
  # Highlight the symbol and its references when holding the cursor
  autocmd CursorHold * silent call CocActionAsync('highlight')
  # Setup formatexpr for specific filetypes
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  # Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

# -----------------------------------------------------------------------------
# 自定义命令
# -----------------------------------------------------------------------------
command! -nargs=0 Format :call CocActionAsync('format')
command! -nargs=? Fold   :call CocAction('fold', <f-args>)
command! -nargs=0 OR     :call CocActionAsync('runCommand', 'editor.action.organizeImport')

# -----------------------------------------------------------------------------
# 状态栏集成
# -----------------------------------------------------------------------------
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
