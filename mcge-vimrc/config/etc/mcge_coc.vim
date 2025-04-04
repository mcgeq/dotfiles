vim9script
# 配置coc配置目录
g:coc_config_home = g:mcge_customvimrcdir .. "/config"
# 配置coc安装extensions位置
g:coc_data_home = g:mcge_customvimrcdir .. "/coc-data"

# 安装的插件

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
    "coc-yank",
\]

# https://raw.githubusercontent.com/neoclide/coc.nvim/master/doc/coc-example-config.vim

# Highlight the symbol and its references when holding the cursor
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
  autocmd!
  # Setup formatexpr specified filetype(s)
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  # Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

# Add `:Format` command to format current buffer
command! -nargs=0 Format :call CocActionAsync('format')

# Add `:Fold` command to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

# Add `:OR` command for organize imports of the current buffer
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

# Add (Neo)Vim's native statusline support
# NOTE: Please see `:h coc-status` for integrations with external plugins that
# provide custom statusline: lightline.vim, vim-airline
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

# 补全高亮
autocmd CursorHold * silent call CocActionAsync('highlight')
