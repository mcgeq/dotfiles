vim9script
# ============================================================================
# vim-clap 配置 - 模糊搜索增强
# ============================================================================

# 配置 ripgrep 作为默认 grep 程序
if executable('rg')
  set grepprg=rg\ --vimgrep\ --no-hidden\ --smart-case\ --glob=!node_modules/*\ --glob=!*.lock
  set grepformat=%f:%l:%c:%m
endif

# vim: set ft=vim sw=2 ts=2 sts=2 et:
