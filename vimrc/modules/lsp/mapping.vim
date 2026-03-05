vim9script
# ============================================================================
# LSP 模块 - CoC 快捷键映射
# 作者：mcge <mcgeq@outlook.com>
# ============================================================================

# 设置 CoC 相关快捷键
def SetupCocMappings()
  # -------------------- coc-explorer --------------------
  nmap <leader>e :CocCommand explorer<CR>
  nmap <leader>ed <Cmd>CocCommand explorer --preset .vim<CR>
  nmap <leader>ef <Cmd>CocCommand explorer --preset floating<CR>
  nmap <leader>ec <Cmd>CocCommand explorer --preset cocConfig<CR>
  nmap <leader>eb <Cmd>CocCommand explorer --preset buffer<CR>
  nmap <leader>el <Cmd>CocList explPresets<CR>

  # -------------------- coc-yank --------------------
  nnoremap <silent> <leader>y :<C-u>CocList -A --normal yank<CR>

  # -------------------- 诊断导航 --------------------
  nmap <silent> [g <Plug>(coc-diagnostic-prev)
  nmap <silent> ]g <Plug>(coc-diagnostic-next)

  # -------------------- GoTo 定义 --------------------
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)

  # -------------------- 文档悬停 --------------------
  nnoremap <silent> K :call CocActionAsync('doHover')<CR>
  inoremap <silent> <C-k> <Esc>:call CocActionAsync('doHover')<CR>

  # -------------------- 符号重命名 --------------------
  nmap <leader>rn <Plug>(coc-rename)

  # -------------------- 格式化 --------------------
  xmap <leader>f  <Plug>(coc-format-selected)
  nmap <leader>f  <Plug>(coc-format-selected)

  # -------------------- 代码操作 --------------------
  xmap <leader>a  <Plug>(coc-codeaction-selected)
  nmap <leader>a  <Plug>(coc-codeaction-selected)
  nmap <leader>ac  <Plug>(coc-codeaction-cursor)
  nmap <leader>as  <Plug>(coc-codeaction-source)
  nmap <leader>qf  <Plug>(coc-fix-current)
  nmap <silent> <leader>re <Plug>(coc-codeaction-refactor)
  xmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)
  nmap <silent> <leader>r  <Plug>(coc-codeaction-refactor-selected)
  nmap <leader>cl  <Plug>(coc-codelens-action)

  # -------------------- 函数和类文本对象 --------------------
  xmap if <Plug>(coc-funcobj-i)
  omap if <Plug>(coc-funcobj-i)
  xmap af <Plug>(coc-funcobj-a)
  omap af <Plug>(coc-funcobj-a)
  xmap ic <Plug>(coc-classobj-i)
  omap ic <Plug>(coc-classobj-i)
  xmap ac <Plug>(coc-classobj-a)
  omap ac <Plug>(coc-classobj-a)

  # -------------------- 滚动浮动窗口 --------------------
  if has('nvim-0.4.0') || has('patch-8.2.0750')
    nnoremap <silent><nowait><expr> <M-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    nnoremap <silent><nowait><expr> <M-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
    inoremap <silent><nowait><expr> <M-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
    inoremap <silent><nowait><expr> <M-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
    vnoremap <silent><nowait><expr> <M-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
    vnoremap <silent><nowait><expr> <M-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  endif

  # -------------------- 选择范围 --------------------
  nmap <silent> <M-s> <Plug>(coc-range-select)
  xmap <silent> <M-s> <Plug>(coc-range-select)

  # -------------------- CoCList 快捷命令 --------------------
  nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
  nnoremap <silent><nowait> <space>l  :<C-u>CocList extensions<cr>
  nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
  nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
  nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
  nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
  nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
  nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>
enddef

# 设置补全映射
def SetupCompletionMappings()
  # Tab 补全
  inoremap <silent><expr> <TAB>
        \ coc#pum#visible() ? coc#pum#next(1) :
        \ g:CheckBackspace() ? "\<Tab>" :
        \ coc#refresh()
  inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"

  # CR 确认
  inoremap <silent><expr> <CR> coc#pum#visible() ? coc#pum#confirm()
                                \ : "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

  # C-space 触发补全
  if has('nvim')
    inoremap <silent><expr> <c-space> coc#refresh()
  else
    inoremap <silent><expr> <c-@> coc#refresh()
  endif
enddef

# 设置 CoC List 映射
def SetupCocListMappings()
  nnoremap <silent> <leader>ff :CocList files<CR>
  nnoremap <silent> <leader>fw :CocList grep<CR>
  nnoremap <silent> <leader>fo :CocList outline<CR>
  nnoremap <silent> <leader>fb :CocList buffers<CR>
  nnoremap <silent> <leader>fr :CocList mru<CR>
  nnoremap <silent> <leader>fc :CocList cmdhistory<CR>
  nnoremap <silent> <leader>gf :CocList gfiles<CR>
  nnoremap <silent> <leader>fl :CocListResume<CR>
enddef

# 立即设置映射
SetupCocMappings()
SetupCompletionMappings()
SetupCocListMappings()

# vim: set ft=vim sw=2 ts=2 sts=2 et:
