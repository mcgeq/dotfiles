vim9script

set number          	# 显示行号
set relativenumber  	# 显示相对行号
set smartindent     	# 智能缩进
set autoindent      	# 自动换行缩进
set linebreak       	# 软折行
set noswapfile      	# 禁止生成 swap 文件
set hidden				# 终端隐藏后不结束
set ignorecase			# 忽略大小写
set infercase
set history=500			# 历史命令
set splitbelow			# 在下方分割	
set smarttab
set shiftround
set wildmenu            # 使用tab补全
set backspace=indent,eol,start

set nobackup
set nowritebackup
set shortmess+=c
set sessionoptions+=globals
set autoread				# 文件外部修改过重新载入
set autowrite				# 自动写回
set confirm					# 显示确认对话框
set noshowmode
set ttimeout
set timeoutlen=499
set ttimeoutlen=99
set updatetime=300			# 更新时间 100ms
set mouse=n					# 允许 normal 模式使用鼠标,a 是全模式

set lazyredraw				# 延迟绘制，提升性能

set showmatch               # 高亮显示匹配的括号
set matchtime=1             # 高亮括号的闪烁时间
# set ffs=unix,doc,mac		# 文件换行符
set clipboard+=unnamedplus	# 与系统共用剪切板
set fileformat=unix

# coc
# Always show the signcolumn, otherwise it would shift the text each time
# diagnostics appear/become resolved
set signcolumn=yes

# 错误格式
set errorformat+=[%f:%l]\ ->\ %m,[%f:%l]:%m

# ----------font---------------
if has('gui_running')
	# set guifont=Fira_Code:h12:cANSI:qDRAFT
    if has('win32') || has('win64')
        set guifont=FiraMono_Nerd_Font_Mono:h12,FiraCode_Nerd_Font_Mono:h12,Consolas:h12
    else
        set guifont=FiraCode_Nerd_Font_Mono:h12
    endif
endif

# 基本配置
if &compatible
    # 不兼容 vi
    set nocompatible
endif

# 语法高亮设置
if has('syntax')
	syntax enable
	syntax on
endif

# 允许 Vim 自带脚本根据文件类型自动设置缩进等
if has('autocmd')
	filetype plugin indent on
endif

# 编码设置
if has('multi_byte')
    # 内部工作编码
	set encoding=utf-8
    # 文件默认编码
	set fileencoding=utf-8
    # 打开文件时自动尝试以下顺序的编码
	set fileencodings=ucs-bom,utf-8,gbk,gb18030,cp936,latin1
endif

if has('win32') || has('win64') || has('win95')
    # Windows 禁用 ALT 操作菜单
    set winaltkeys=no
endif

# 设置代码折叠
if has('folding')
    # 允许代码折叠
	set foldenable
    # 代码折叠默认使用缩进
	set foldmethod=indent # 基于缩进的折叠
	set foldlevel=99		# 默认打开所有缩进
endif


if has("autocmd")
	au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif | normal! zvzz
endif

if has('gui_running')
    set guicursor+=a:ver25
else
	set guicursor+=a:ver25
endif


# 文件搜索和补全时忽略如下的扩展名
set suffixes=.bak,~,.o,.h,.info,.swp,.obj,.pyc,.pyo,.egg-info,.class
set wildignore=*.o,*.obj,*~,*.exe,*.a,*.pdb,*.lib
set wildignore+=*.so,*.dll,*.swp,*.egg,*.jar,*.class,*.pyc,*.pyo,*.bin,*.dex
# MacOSX/Linux
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
