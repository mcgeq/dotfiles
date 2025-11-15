vim9script

# -------------------- vim-clap start --------------------
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
# -------------------- vim-clap   end --------------------

# --------------------  fzf     start --------------------
nnoremap <silent><nowait> <C-l> :<c-u>Lines<CR>
inoremap <silent><nowait> <C-l> <Esc>:<c-u>Lines<CR>
vnoremap <silent><nowait> <C-l> :<c-u>Lines<CR>
nnoremap <silent><nowait> <C-s> :<c-u>BLines<CR>
inoremap <silent><nowait> <C-s> <Esc>:<c-u>BLines<CR>
vnoremap <silent><nowait> <C-s> :<c-u>BLines<CR>
# --------------------      fzf   end --------------------

# -------------------- vista start --------------------
nnoremap <silent><nowait> <leader>v :<c-u>Vista<CR>             # 打开Vista窗口
nnoremap <silent><nowait> <leader>vb :<c-u>Vista!!<CR>          # 更新Vista窗口
nnoremap <silent><nowait> <leader>vc :<c-u>Vista focus<CR>      # 聚焦Vista窗口
nnoremap <silent><nowait> <leader>ve :<c-u>Vista finder<CR>     # 在Vista窗口中搜索
nnoremap <silent><nowait> <leader>vr :<c-u>Vista refresh<CR>    # 刷新Vista窗口
nnoremap <silent><nowait> <leader>vt :<c-u>Vista kind<CR>       # 选择Vista窗口显示的符号类型
# -------------------- vista   end --------------------
