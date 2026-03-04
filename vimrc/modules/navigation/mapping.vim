vim9script
# ============================================================================
# 导航模块 - 快捷键映射
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 设置导航相关快捷键
def SetupNavigationMappings()
  # -------------------- Clap 前缀映射 --------------------
  # 使用 <leader>o 作为导航前缀（open 的首字母）
  nnoremap <silent><nowait> <leader>op :<c-u>Clap<CR>
  nnoremap <silent><nowait> <leader>ob :<c-u>Clap buffers<CR>
  nnoremap <silent><nowait> <leader>oc :<c-u>Clap command<CR>
  nnoremap <silent><nowait> <leader>oh :<c-u>Clap history<CR>
  nnoremap <silent><nowait> <leader>of :<c-u>Clap files<CR>
  nnoremap <silent><nowait> <leader>df :<c-u>Clap filer<CR>
  nnoremap <silent><nowait> <leader>oq :<c-u>Clap quickfix<CR>
  nnoremap <silent><nowait> <leader>oj :<c-u>Clap jumps<CR>
  nnoremap <silent><nowait> <leader>om :<c-u>Clap marks<CR>
  nnoremap <silent><nowait> <leader>ow :<c-u>Clap windows<CR>
  nnoremap <silent><nowait> <leader>ot :<c-u>Clap tags<CR>
  nnoremap <silent><nowait> <leader>os :<c-u>Clap colors<CR>
  nnoremap <silent><nowait> <leader>og :<c-u>Clap igrep<CR>
  nnoremap <silent><nowait> <leader>or :<c-u>Clap recent_files<CR>

  # -------------------- Vista 前缀映射 --------------------
  # 使用 <leader>v 作为 Vista 前缀
  nnoremap <silent><nowait> <leader>v :<c-u>Vista<CR>
  nnoremap <silent><nowait> <leader>vb :<c-u>Vista!!<CR>
  nnoremap <silent><nowait> <leader>vc :<c-u>Vista focus<CR>
  nnoremap <silent><nowait> <leader>ve :<c-u>Vista finder<CR>
  nnoremap <silent><nowait> <leader>vr :<c-u>Vista refresh<CR>
  nnoremap <silent><nowait> <leader>vt :<c-u>Vista kind<CR>
enddef

# 立即设置映射
SetupNavigationMappings()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
