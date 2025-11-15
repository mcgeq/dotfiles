vim9script

# -------------------- git start --------------------
nnoremap <silent><nowait> <leader>gg  :G<CR>
# q 关闭窗口
# A 调整大小至作者
# C 调整大小至提交列
# D 调整大小至日期/时间列
nnoremap <silent><nowait> <leader>gb  :<c-u>Git blame<CR>
nnoremap <silent><nowait> <leader>gd  :<c-u>Gdiffsplit<CR>
nnoremap <silent><nowait> <leader>gw  :<c-u>Gwrite<CR>
nnoremap <silent><nowait> <leader>gr  :<c-u>Gread<CR>
nnoremap <silent><nowait> <leader>gm  :<c-u>Git commit<CR>
nnoremap <silent><nowait> <leader>gc  :<c-u>Gclog<CR>
nnoremap <silent><nowait> <leader>go  :<c-u>copen<CR>
nnoremap <silent><nowait> <leader>gq  :<c-u>cclose<CR>
nnoremap <silent><nowait> <leader>gn  :<c-u>cnext<CR>
nnoremap <silent><nowait> <leader>gp  :<c-u>cprevious<CR>
inoremap <silent><nowait> <leader>gg  <esc>:<c-u>G<CR>
inoremap <silent><nowait> <leader>gb  <Esc>:<c-u>Git blame<CR>
inoremap <silent><nowait> <leader>gd  <Esc>:<c-u>Gdiffsplit<CR>
inoremap <silent><nowait> <leader>gw  <Esc>:<c-u>Gwrite<CR>
inoremap <silent><nowait> <leader>gr  <Esc>:<c-u>Gread<CR>
inoremap <silent><nowait> <leader>gm  <Esc>:<c-u>Git commit<CR>
inoremap <silent><nowait> <leader>gc  <Esc>:<c-u>Gclog<CR>
# -------------------- git end --------------------
