# mcge 的 Vim 配置

> **版本 2.0** - 现代化、模块化、高性能

[English Documentation](./README.md)

基于 **Vim9script** 的生产级 Vim 配置，采用混合模块化架构、智能懒加载和完整的 CoC.nvim LSP 支持。

---

## ✨ 特性

### 🏗️ 混合架构 (v2.0)

```
Bootstrap → Core → Modules → Config → Local
   ↓         ↓        ↓         ↓        ↓
  环境     功能     功能模块   简单配置  用户配置
```

- **Bootstrap** - 环境初始化、常量定义、基础设置
- **Core** - 错误处理、工具函数、模块加载器、健康检查
- **Modules** - 功能模块，带配置字典和健康检查（20 个文件）
- **Config** - 简单配置、语言特定设置（8 个文件）
- **Local** - 用户自定义配置（不被 Git 跟踪）

### ⚡ 性能优化

- **智能懒加载** - 模块按需加载，启动速度 < 100ms
- **性能监控** - 内置启动时间追踪和模块加载统计
- **健康检查** - 自动检测配置、依赖和插件状态
- **延迟初始化** - 非关键模块延迟加载

### 🌐 语言支持 (45+ CoC 扩展)

- **系统级**：C/C++ (clangd), Java, CMake, Zig
- **Web**：TypeScript/JavaScript, HTML, CSS/SCSS, Vue 2/3, React, TailwindCSS v3
- **动态**：Python (Pyright), Lua, Clojure, Shell
- **系统**：Rust (rust-analyzer)
- **数据**：JSON, YAML, TOML, XML, SQL
- **工具**：Git, Prettier, ESLint, AI 补全 (TabNine)

### 🔧 核心插件

**LSP 与补全**
- CoC.nvim - 完整的 LSP 支持，45+ 扩展自动安装

**搜索与导航**
- **Clap** - 现代化模糊搜索（比 FZF 快）
- **Vista** - 代码大纲和符号导航
- **CoC Explorer** - 文件浏览器

**UI 与外观**
- **Startify** - 启动界面
- **Airline** - 状态栏
- **Which-key** - 快捷键提示

**编辑增强**
- **Floaterm** - 浮动终端
- **vim-visual-multi** - 多光标编辑
- **NERDCommenter** - 注释
- **vim-matchup** - 增强匹配

---

## 📦 安装

### 系统要求

**必需**
- Vim 9.0+ 或 Neovim 0.8+
- Node.js 16+ (CoC.nvim)
- Git

**推荐**
- `ripgrep` (rg) - 快速文本搜索
- `fd` - 快速文件查找
- `ctags` - 代码标签（Vista）

### Windows 安装

```powershell
# 1. 安装推荐工具
winget install BurntSushi.ripgrep.MSVC
winget install sharkdp.fd
winget install UniversalCtags.UniversalCtags

# 2. 克隆配置
git clone <your-repo> vimrc

# 3. 运行安装脚本
cd vimrc
.\install.bat
```

### Linux/macOS 安装

```bash
# 1. 安装推荐工具
# Ubuntu/Debian
sudo apt install ripgrep fd-find universal-ctags

# macOS
brew install ripgrep fd ctags

# 2. 克隆配置
git clone <your-repo> vimrc

# 3. 运行安装脚本
cd vimrc
chmod +x install.sh
./install.sh
```

### 首次启动

1. 启动 Vim：`vim`
2. CoC 扩展会自动安装（首次启动需要几分钟）
3. 运行健康检查：`:VimrcLoadReport`
4. 查看启动时间：`:VimStartupTime`

---

## 🗂️ 目录结构

### 混合架构

```
vimrc/
├── init.vim                    # 主配置入口
├── bootstrap/                  # 环境初始化与基础设置
│   ├── constants.vim           # 全局常量
│   ├── environment.vim         # 环境检测
│   └── settings.vim            # 基础 Vim 设置
├── core/                       # 核心功能
│   ├── error_handler.vim       # 错误处理
│   ├── utils.vim               # 工具函数
│   ├── loader.vim              # 模块加载器
│   └── health.vim              # 健康检查系统
├── modules/                    # 功能模块（20 个文件）
│   ├── editor/ (7)             # 编辑器增强
│   │   ├── commenter.vim       # NERDCommenter
│   │   ├── match-pair.vim      # vim-matchup
│   │   ├── multi-cursor.vim    # vim-visual-multi
│   │   ├── snippets.vim        # UltiSnips
│   │   ├── tabsize.vim         # Tab 配置
│   │   ├── tags.vim            # gutentags
│   │   └── whitespace.vim      # 空白高亮
│   ├── git/ (2)                # Git 集成
│   │   ├── gutter.vim          # vim-gitgutter
│   │   └── mapping.vim         # Git 快捷键
│   ├── lsp/ (2)                # LSP 配置
│   │   ├── coc.vim             # CoC 配置
│   │   └── mapping.vim         # LSP 快捷键
│   ├── navigation/ (3)         # 导航
│   │   ├── clap.vim            # Clap 模糊搜索
│   │   ├── mapping.vim         # 导航快捷键
│   │   └── vista.vim           # Vista 大纲
│   ├── terminal/ (1)           # 终端
│   │   └── floaterm.vim        # Floaterm
│   └── ui/ (5)                 # UI 配置
│       ├── airline.vim         # Airline 状态栏
│       ├── appearance.vim      # GUI 设置
│       ├── colorscheme.vim     # 配色方案
│       ├── startify.vim        # 启动界面
│       └── statusline.vim      # 状态栏配置
├── config/                     # 简单配置（8 个文件）
│   ├── lang/ (6)               # 语言特定配置
│   │   ├── python.vim
│   │   ├── rust.vim
│   │   ├── typescript.vim
│   │   ├── zig.vim
│   │   ├── css.vim
│   │   └── html.vim
│   ├── mapping/
│   │   └── basic.vim           # 基础全局快捷键
│   ├── ftplugin/
│   │   └── vim.vim             # Vim ftplugin
│   └── coc-settings.json       # CoC JSON 配置
├── local/                      # 用户自定义配置
│   ├── user_env.vim            # 用户环境变量
│   ├── user_settings.vim       # 用户设置
│   └── user_mappings.vim       # 用户快捷键
└── pack/mcge/start/            # Vim 插件
```

**设计原则：**
- **modules/** - 复杂功能，带配置字典、函数封装、健康检查
- **config/** - 简单设置、语言特定 autocmds、基础快捷键

---

## ⌨️ 快捷键

### Leader 键：`<Space>`

### Startify 启动界面

| 键 | 功能 |
|----|------|
| `n` | 新建文件 |
| `f` | 文件搜索 (Clap) |
| `o` | 最近文件 |
| `w` | 文本搜索 |
| `s` | 加载会话 |
| `c` | 打开配置 |

### Clap 搜索

| 快捷键 | 功能 |
|--------|------|
| `<leader>p` | 文件搜索 |
| `<leader>P` | Git 文件 |
| `<leader>/` | 文本搜索 |
| `<leader>fg` | Grep 搜索 |
| `<leader>bb` | 缓冲区 |
| `<leader>fh` | 最近文件 |
| `<leader>fl` | 当前文件行 |
| `<leader>gc` | Git 提交 |
| `<leader>:` | 命令搜索 |
| `<leader>;` | 命令历史 |
| `<leader>km` | 快捷键映射 |
| `<leader>?` | 帮助标签 |
| `<leader>tc` | 配色方案 |

### Vista 代码大纲

| 快捷键 | 功能 |
|--------|------|
| `<F8>` | 打开/关闭 |
| `<leader>v` | 打开/关闭 |
| `<leader>vf` | 符号搜索 |
| `<leader>vc` | 使用 CoC 后端 |
| `<leader>vt` | 使用 ctags 后端 |

### CoC LSP

**代码导航**
- `gd` - 跳转到定义
- `gy` - 跳转到类型定义
- `gi` - 跳转到实现
- `gr` - 查找引用
- `K` - 显示文档

**代码操作**
- `<leader>rn` - 重命名符号
- `<leader>f` - 格式化代码
- `<leader>a` - 代码操作
- `<leader>ac` - 代码操作（光标）
- `[g` / `]g` - 上一个/下一个诊断

**CoC Explorer**
- `<leader>e` - 打开浏览器
- `<leader>ed` - 浏览器（目录）
- `<leader>ef` - 浏览器（浮动）
- `<leader>y` - 剪切历史

### 窗口和缓冲区

- `<Ctrl-h/j/k/l>` - 切换窗口
- `<Ctrl-tab>` / `<Ctrl-s-tab>` - 下一个/上一个缓冲区
- `<Ctrl-x><Ctrl-s>` - 保存文件
- `<Ctrl-x><Ctrl-q>` - 保存并退出

---

## ⚙️ 配置

### 用户环境变量

编辑 `local/user_env.vim`：

```vim
vim9script

# 作者信息
g:mcge_custom_author = "你的名字"
g:mcge_custom_email = "<your@email.com>"

# Vista 后端（coc 或 ctags）
g:mcge_custom_vista_executive = "coc"

# Windows 路径
if has('win32') || has('win64')
  g:mcge_custom_project = "E:/MyProjects"
  g:mcge_custom_workspace = "E:/Workspaces"
endif
```

### 用户设置

编辑 `local/user_settings.vim`：

```vim
vim9script

# 你的自定义设置
set number
set relativenumber
```

### 用户快捷键

编辑 `local/user_mappings.vim`：

```vim
vim9script

# 你的自定义快捷键
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
```

### CoC 配置

编辑 `config/coc-settings.json`：

```json
{
  "python.linting.enabled": true,
  "python.formatting.provider": "black",
  "rust-analyzer.checkOnSave.command": "clippy"
}
```

---

## 🔧 常用命令

### 性能和调试

```vim
:VimStartupTime          " 查看启动时间
:VimrcLoadReport         " 模块加载报告
:CheckHealth             " 健康检查
```

### Clap 搜索

```vim
:Clap files              " 文件搜索
:Clap grep               " 文本搜索
:Clap buffers            " 缓冲区
:Clap history            " 最近文件
:Clap command            " 命令
:Clap command_history    " 命令历史
:Clap maps               " 快捷键映射
:Clap help_tags          " 帮助标签
:Clap colors             " 配色方案
```

### Vista 大纲

```vim
:Vista                   " 打开/关闭
:Vista finder            " 符号搜索
:Vista coc               " 使用 CoC 后端
:Vista ctags             " 使用 ctags 后端
:Vista info              " 显示信息
```

### CoC

```vim
:CocInfo                 " CoC 信息
:CocList extensions      " 扩展列表
:CocCommand explorer     " 文件浏览器
:Format                  " 格式化
:OR                      " 组织导入
:CocRestart              " 重启 CoC
```

---

## 🐛 故障排除

### CoC 不工作

1. 检查 Node.js：`node --version`（需要 16+）
2. 查看状态：`:CocInfo`
3. 重启 CoC：`:CocRestart`
4. 检查扩展：`:CocList extensions`

### 搜索很慢

1. 确保安装了 `ripgrep`：`rg --version`
2. 使用 Clap：`:Clap files`
3. 使用 CoC List：`:CocList files`

### Vista 不显示符号

1. 确认文件类型：`:Vista info`
2. 切换后端：`:Vista coc`
3. 检查 CoC：`:CocInfo`

### 模块加载问题

```vim
:VimrcLoadReport         " 检查哪些模块加载失败
:echo g:mcge_startup_time " 查看启动时间（毫秒）
```

---

## 🚀 性能指标

- **启动时间**：~80-100ms
- **模块数量**：20 个功能模块
- **CoC 扩展**：45+

### 查看性能

```vim
:VimStartupTime          " 启动时间
:VimrcLoadReport         " 模块加载报告
:CheckHealth             " 健康状况
```

---

## 📚 进阶使用

### 添加插件

```bash
cd pack/mcge/start
git clone https://github.com/author/plugin-name
```

### 添加 CoC 扩展

```vim
:CocInstall coc-extension-name
:CocUninstall coc-extension-name
```

### 模块健康检查

每个模块都提供健康检查函数：

```vim
:call g:ClapHealthCheck()
:call g:VistaHealthCheck()
:call g:AirlineHealthCheck()
```

---

## 🔗 相关资源

- [Vim 9 文档](https://vimhelp.org/vim9.txt.html)
- [CoC.nvim](https://github.com/neoclide/coc.nvim)
- [vim-clap](https://github.com/liuchengxu/vim-clap)
- [Vista.vim](https://github.com/liuchengxu/vista.vim)
- [vim-airline](https://github.com/vim-airline/vim-airline)
- [vim-startify](https://github.com/mhinz/vim-startify)

---

## 📄 许可证

MIT License

## 🙏 致谢

- CoC.nvim 团队
- 所有插件作者
- Vim 社区

---

**享受你的 Vim 之旅！** 🎉
