vim9script
# ============================================================================
# vim-gutentags 配置 - 自动生成 tags
# ============================================================================

g:gutentags_cache_dir = g:mcge_customvimrcdir .. '/cache/tags'

g:gutentags_project_root = ['.git', '.hg', '.svn', 'CMakeLists.txt', 'Makefile', 'package.json']

g:gutentags_generate_on_new = 1
g:gutentags_generate_on_missing = 1
g:gutentags_generate_on_save = 1
g:gutentags_generate_on_empty_buffer = 0

g:gutentags_exclude = ['*.txt', '*.md', '*.json', '*.yaml', '*.toml', '*/tmp/*', '*/node_modules/*', '*/.git/*']

g:gutentags_ctags_autotag = 1

# vim: set ft=vim sw=2 ts=2 sts=2 et:
