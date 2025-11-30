# mcge 的 Vim 配置

> **版本 2.0** - 现代化、模块化、高性能

[English Documentation](./README.md)

基于 **Vim9script** 的生产级 Vim 配置，具有模块化架构、智能懒加载和完整的 CoC.nvim LSP 支持。

---

## ✨ 特性

### 🏗️ 模块化架构 (v2.0)

```
Bootstrap → Core → Modules → Config → Local
   ↓         ↓        ↓         ↓        ↓
  环境     功能    UI/LSP   兼容层   用户配置
```

- **Bootstrap** - 环境初始化、常量定义、基础设置
- **Core** - 错误处理、工具函数、模块加载器、健康检查
- **Modules** - UI 配置、LSP 配置（模块化、可插拔）
- **Config** - 兼容旧配置、插件配置、按键映射
- **Local** - 用户自定义配置（不被 Git 跟踪）

### ⚡ 性能优化

- **智能懒加载** - UI、LSP 按需加载，启动速度 < 100ms
- **性能监控** - 内置启动时间追踪和模块加载统计
- **健康检查** - 自动检测配置、依赖和插件状态
- **延迟初始化** - Statusline、非关键模块延迟加载

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
- **vim-surround** - 快速包围
- **vim-commentary** - 注释

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
3. 运行健康检查：`:CheckHealth`
4. 查看启动时间：`:VimStartupTime`

---

## 🗂️ 目录结构

```
vimrc/
├── init.vim                # 主配置入口
├── bootstrap/              # 环境初始化与基础设置
├── core/                   # 错误处理、工具函数、加载器、健康检查
├── modules/                # UI 与 LSP 模块（模块化、可插拔）
├── config/                 # 插件配置、按键映射、语言特定配置
├── local/                  # 用户自定义配置（不被 Git 跟踪）
└── pack/                   # Vim 插件
```

**架构分层**: Bootstrap → Core → Modules → Config → Local

---

## ⌨️ 快捷键

### Leader 键: `<Space>`

### Startify 启动界面

| 键 | 功能 |
|----|------|
| `n` | 新建文件 |
| `f` | 文件搜索 (Clap) |
| `o` | 最近文件 |
| `w` | 文本搜索 |
| `s` | 加载会话 |
| `c` | 打开配置 |

### Clap 搜索（推荐）

| 快捷键 | 功能 |
|--------|------|
| `<leader>p` | 文件搜索 |
| `<leader>P` | Git 文件 |
| `<leader>/` | 文本搜索 |
| `<leader>bb` | 缓冲区 |
| `<leader>fh` | 最近文件 |
| `<leader>gc` | Git 提交 |

### Vista 代码大纲

| 快捷键 | 功能 |
|--------|------|
| `<F8>` | 打开/关闭 |
| `<leader>v` | 打开/关闭 |
| `<leader>vf` | 符号搜索 |

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
- `[g` / `]g` - 上一个/下一个诊断

### 窗口和缓冲区

- `<Ctrl-h/j/k/l>` - 切换窗口
- `<Ctrl-n/p>` - 切换缓冲区
- `<Ctrl-x><Ctrl-s>` - 保存文件

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
```

### Vista 大纲

```vim
:Vista                   " 打开/关闭
:Vista finder            " 符号搜索
:Vista coc               " 使用 CoC 后端
```

### CoC

```vim
:CocInfo                 " CoC 信息
:CocList extensions      " 扩展列表
:CocCommand explorer     " 文件浏览器
:Format                  " 格式化
:OR                      " 组织导入
```

---

## 🐛 故障排除

### CoC 不工作

1. 检查 Node.js：`node --version`（需要 16+）
2. 查看状态：`:CocInfo`
3. 重启 CoC：`:CocRestart`

### 搜索很慢

1. 确保安装了 `ripgrep`：`rg --version`
2. 使用 Clap：`:Clap files`
3. 使用 CoC List：`:CocList files`

### Vista 不显示符号

1. 确认文件类型：`:Vista info`
2. 切换后端：`:Vista coc`
3. 检查 CoC：`:CocInfo`

---

## 🚀 性能指标

- **启动时间**：~80-100ms
- **模块数量**：40+
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
```

### 自定义快捷键

编辑 `local/user_mappings.vim`：

```vim
vim9script

nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
```

---

## 🔗 相关资源

- [Vim 9 文档](https://vimhelp.org/vim9.txt.html)
- [CoC.nvim](https://github.com/neoclide/coc.nvim)
- [vim-clap](https://github.com/liuchengxu/vim-clap)
- [Vista.vim](https://github.com/liuchengxu/vista.vim)

---

## 📄 许可证

MIT License

## 🙏 致谢

- CoC.nvim 团队
- 所有插件作者
- Vim 社区

---

**享受你的 Vim 之旅！** 🎉
