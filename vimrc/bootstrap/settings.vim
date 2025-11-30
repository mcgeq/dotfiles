vim9script
# ============================================================================
# Vim 基础设置
# 作者: mcge <mcgeq@outlook.com>
# 描述: 从 mcge_base.vim 迁移的基础配置
# ============================================================================

# ----------------------------------------------------------------------------
# 基本配置
# ----------------------------------------------------------------------------
if &compatible
    set nocompatible
endif

# 显示设置
set number
set relativenumber
set showmatch
set matchtime=1
set noshowmode
set lazyredraw
set signcolumn=yes

# 编辑行为
set smartindent
set autoindent
set linebreak
set backspace=indent,eol,start

# 文件处理
set noswapfile
set nobackup
set nowritebackup
set hidden
set autoread
set autowrite
set confirm
setglobal fileformat=unix  # 使用 setglobal 避免修改只读 buffer

# 搜索设置
set ignorecase
set infercase

# 历史和命令
set history=500
set wildmenu
set shortmess+=c

# 分割窗口
set splitbelow

# Tab 设置
set smarttab
set shiftround

# 会话选项
set sessionoptions+=globals

# 性能优化
set updatetime=300
set timeoutlen=499
set ttimeoutlen=99
set ttimeout
set lazyredraw

# 鼠标支持（仅 normal 模式）
set mouse=n

# 剪贴板
set clipboard+=unnamedplus

# 错误格式
set errorformat+=[%f:%l]\ ->\ %m,[%f:%l]:%m

# ----------------------------------------------------------------------------
# 字体设置
# ----------------------------------------------------------------------------
if has('gui_running')
    if has('win32') || has('win64')
        set guifont=FiraCode_Nerd_Font_Mono:h12,FiraMono_Nerd_Font_Mono:h12,Consolas:h12
    else
        set guifont=FiraCode_Nerd_Font_Mono:h12
    endif
endif

# ----------------------------------------------------------------------------
# 语法高亮
# ----------------------------------------------------------------------------
if has('syntax')
	syntax enable
	syntax on
endif

# ----------------------------------------------------------------------------
# 文件类型检测
# ----------------------------------------------------------------------------
if has('autocmd')
	filetype plugin indent on
endif

# ----------------------------------------------------------------------------
# 编码设置
# ----------------------------------------------------------------------------
if has('multi_byte')
	set encoding=utf-8
	setglobal fileencoding=utf-8      # 使用 setglobal 避免修改只读 buffer
	set fileencodings=ucs-bom,utf-8,gbk,gb18030,cp936,latin1
endif

# ----------------------------------------------------------------------------
# Windows 特定设置
# ----------------------------------------------------------------------------
if has('win32') || has('win64') || has('win95')
    set winaltkeys=no
endif

# ----------------------------------------------------------------------------
# 代码折叠
# ----------------------------------------------------------------------------
if has('folding')
	set foldenable
	set foldmethod=indent
	set foldlevel=99
endif

# ----------------------------------------------------------------------------
# 光标设置
# ----------------------------------------------------------------------------
if has('gui_running')
    set guicursor+=a:ver25
else
	set guicursor+=a:ver25
endif

# ----------------------------------------------------------------------------
# 自动命令
# ----------------------------------------------------------------------------
if has("autocmd")
	# 打开文件时恢复光标位置
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif | normal! zvzz
endif

# ----------------------------------------------------------------------------
# 文件搜索忽略模式
# ----------------------------------------------------------------------------
set suffixes=.bak,~,.o,.h,.info,.swp,.obj,.pyc,.pyo,.egg-info,.class

set wildignore=*.o,*.obj,*~,*.exe,*.a,*.pdb,*.lib
set wildignore+=*.so,*.dll,*.swp,*.egg,*.jar,*.class,*.pyc,*.pyo,*.bin,*.dex
set wildignore+=*.zip,*.7z,*.rar,*.gz,*.tar,*.gzip,*.bz2,*.tgz,*.xz
set wildignore+=*DS_Store*,*.ipch
set wildignore+=*.gem
set wildignore+=*.png,*.jpg,*.gif,*.bmp,*.tga,*.pcx,*.ppm,*.img,*.iso
set wildignore+=*.so,*.swp,*.zip,*/.Trash/**,*.pdf,*.dmg,*/.rbenv/**
set wildignore+=*/.nx/**,*.app,*.git,.git
set wildignore+=*.wav,*.mp3,*.ogg,*.pcm
set wildignore+=*.mht,*.suo,*.sdf,*.jnlp
set wildignore+=*.chm,*.epub,*.pdf,*.mobi,*.ttf
set wildignore+=*.mp4,*.avi,*.flv,*.mov,*.mkv,*.swf,*.swc
set wildignore+=*.ppt,*.pptx,*.docx,*.xlt,*.xls,*.xlsx,*.odt,*.wps
set wildignore+=*.msi,*.crx,*.deb,*.vfd,*.apk,*.ipa,*.bin,*.msu
set wildignore+=*.gba,*.sfc,*.078,*.nds,*.smd,*.smc
set wildignore+=*.linux2,*.win32,*.darwin,*.freebsd,*.linux,*.android

# vim: set ft=vim sw=2 ts=2 sts=2 et:
