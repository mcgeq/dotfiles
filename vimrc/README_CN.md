# mcge 的 Vim 配置

> 现代化、模块化、显式分层

[English Documentation](./README.md)

基于 **Vim9script** 的生产级 Vim 配置，采用清晰且显式的目录架构、智能懒加载和完整的 CoC.nvim LSP 支持。

---

## ✨ 特性

### 🏗️ 混合架构

```
Bootstrap → Core → Modules → Config → Local
   ↓         ↓        ↓         ↓        ↓
  环境     功能     功能模块   简单配置  用户配置
```

- **Bootstrap** - 环境初始化、常量定义、基础设置
- **Core** - 错误处理、工具函数、模块加载器、健康检查
- **Modules** - 承载真实功能初始化逻辑的模块
- **Config** - 共享映射和插件原生静态配置
- **Local** - 用户级覆盖和机器本地变量

语言相关行为现在统一放在 `after/ftplugin`，通过 Vim 原生运行时机制按文件类型加载，而不是额外维护一套自定义语言加载器。

目录职责约定见 [ARCHITECTURE.md](./ARCHITECTURE.md)。

### ⚡ 性能优化

- **智能懒加载** - 只在合适的地方启用延迟加载
- **性能监控** - 内置启动时间追踪和模块加载统计
- **健康检查** - 自动检测配置、依赖和插件状态
- **延迟初始化** - 非关键模块延迟加载

### 🌐 基于 CoC 的语言支持

- **系统级**：C/C++ (clangd), Java, CMake, Zig
- **Web**：TypeScript/JavaScript, HTML, CSS/SCSS, Vue 2/3, React, TailwindCSS v3
- **动态**：Python (Pyright), Lua, Clojure, Shell
- **系统**：Rust (rust-analyzer)
- **数据**：JSON, YAML, TOML, XML, SQL
- **工具**：Git, Prettier, ESLint, AI 补全 (TabNine)

### 🔧 核心插件

**LSP 与补全**
- CoC.nvim - 完整的 LSP 支持，可按项目安装扩展

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
- Vim 9.0+（使用 `vim9script`；Neovim 请使用单独的 `nvim/` 配置）
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
├── ARCHITECTURE.md             # 目录职责约定
├── bootstrap/                  # 环境初始化与基础设置
│   ├── constants.vim           # 全局常量
│   ├── environment.vim         # 环境检测
│   └── settings.vim            # 基础 Vim 设置
├── core/                       # 核心功能
│   ├── error_handler.vim       # 错误处理
│   ├── utils.vim               # 工具函数
│   ├── loader.vim              # 模块加载器
│   ├── keymap.vim              # 声明式按键映射辅助
│   ├── module.vim              # 共享模块辅助
│   └── health.vim              # 健康检查系统
├── modules/                    # 功能模块
│   ├── editor/                 # 编辑器增强
│   │   ├── commenter.vim       # NERDCommenter
│   │   ├── match-pair.vim      # vim-matchup
│   │   ├── multi-cursor.vim    # vim-visual-multi
│   │   ├── snippets.vim        # UltiSnips
│   │   ├── tabsize.vim         # Tab 配置
│   │   ├── tags.vim            # gutentags
│   │   └── whitespace.vim      # 空白高亮
│   ├── git/                    # Git 集成
│   │   └── gutter.vim          # vim-gitgutter
│   ├── lsp/                    # LSP 配置
│   │   └── coc.vim             # CoC 配置
│   ├── navigation/             # 导航
│   │   ├── clap.vim            # Clap 模糊搜索
│   │   └── vista.vim           # Vista 大纲
│   ├── terminal/               # 终端
│   │   └── floaterm.vim        # Floaterm
│   └── ui/                     # UI 配置
│       ├── airline.vim         # Airline 状态栏
│       ├── appearance.vim      # GUI 设置
│       ├── colorscheme.vim     # 配色方案
│       ├── startify.vim        # 启动界面
│       └── whichkey.vim        # Which-key 集成
├── config/                     # 共享配置
│   ├── mapping/                # 统一的按键映射归属
│   │   ├── basic.vim           # 基础全局快捷键
│   │   ├── editor.vim          # 编辑插件快捷键
│   │   ├── git.vim             # Git 快捷键
│   │   ├── lsp.vim             # CoC/LSP 快捷键
│   │   ├── navigation.vim      # Clap/Vista 快捷键
│   │   ├── terminal.vim        # Floaterm 快捷键
│   │   └── ui.vim              # Which-key 触发键
│   └── coc-settings.json       # CoC JSON 配置
├── after/
│   └── ftplugin/               # 标准文件类型局部配置
│       ├── ftplugin.template.vim
│       ├── python.vim
│       ├── rust.vim
│       ├── typescript.vim
│       ├── typescriptreact.vim
│       ├── vim.vim
│       └── zig.vim
├── local/                      # 用户自定义配置
│   ├── user_env.vim            # 用户环境变量
│   ├── module_overrides.example.vim
│   ├── user_mappings.example.vim
│   ├── user_settings.example.vim
│   ├── user_settings.vim       # 用户设置
│   ├── module_overrides.vim    # 可选的模块配置覆盖
│   └── user_mappings.vim       # 可选的个人快捷键
└── pack/mcge/start/            # Vim 插件
```

**设计原则：**
- **modules/** - 复杂功能，包含插件初始化、辅助函数和健康检查
- **config/** - 集中管理共享映射和静态的跨模块配置
- **after/ftplugin/** - 使用 Vim 标准运行时机制承载文件类型局部行为
- **local/** - 放个人覆盖项，不破坏共享模块边界

新增共享模块、文件类型局部配置或本地覆盖时，可以分别从
`modules/module.template.vim`、`after/ftplugin/ftplugin.template.vim`、
`local/module_overrides.example.vim`、`local/user_settings.example.vim`、
`local/user_mappings.example.vim` 开始复制。

**落点规则：**
- 插件初始化和功能逻辑放 `modules/`
- 全局共享按键放 `config/mapping/`，并通过 `core/keymap.vim` 统一注册
- 文件类型局部设置放 `after/ftplugin/`
- `config/` 保持精简、声明式

---

## ⌨️ 快捷键

### Leader 键：`<Space>`

完整快捷键列表由运行时 keymap 注册表自动生成：

- [docs/keymaps.md](./docs/keymaps.md)

该文件是共享映射的单一事实来源。修改
`config/mapping/*.vim` 或 `core/keymap.vim` 后，请执行：

```powershell
pwsh -File vimrc/scripts/verify_keymaps.ps1
```

后续新增 `config/mapping/*.vim` 时，建议统一保持这种文件结构：
先定义共享的 `const ..._defaults`，再定义 which-key 分组，再声明各组 spec
表，最后分别调用一次 `g:MapMany()` / `g:CmdMapMany()` /
`g:PlugMapMany()`。如果某个文件仍需要少量单条映射，优先用
`g:MapSpec(spec, defaults)` 复用默认项，不要把重复参数再写回每条映射里。

代表性命名空间：

- 搜索与导航：`<leader>p`、`<leader>/`、`<leader>sg`、`<leader>sw`
- LSP 与代码动作：`gd`、`gr`、`<leader>rn`、`<leader>af`、`<leader>aa`
- 浏览器与大纲：`<leader>ee`、`<leader>vv`、`<leader>vf`
- 窗口与缓冲区：`<C-h/j/l>`、`<C-tab>`、`<C-x><C-s>`、`<leader>rr`
- 终端：`<F7>`、`<F8>`、`<F9>`、`<F12>`

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

def ApplyUserEditorOverrides()
  set number
  set relativenumber
enddef

def ApplyUserUiOverrides()
  set cursorline
enddef

def RegisterUserAutocmds()
  augroup mcge_local_user_custom
    autocmd!
    autocmd BufEnter *.md setlocal wrap
  augroup END
enddef
```

大多数个人覆盖项都建议直接放在 `local/user_settings.vim`。
建议继续按固定 hook 分区：
`ApplyUserEditorOverrides()`、`ApplyUserUiOverrides()`、
`RegisterUserCommands()`、`RegisterUserAutocmds()`。
如果你希望把个人映射单独隔离出来，也可以自行创建
`local/user_mappings.vim`，但它不是共享架构里的必需文件。

### 模块配置覆盖

如果你只想覆盖共享模块的 `config` 字段，不想直接改 `modules/*`，
请使用 `local/module_overrides.vim`：

```vim
vim9script

g:SetModuleOverride('colorscheme', {scheme: 'desert'})
g:SetModuleOverride('clap', {layout: {width: '90%'}})
```

可以从 `local/module_overrides.example.vim` 复制开始。这是共享模块配置的
首选用户覆盖入口。

### 文件类型局部配置

语言相关的局部配置统一放在 `after/ftplugin/<filetype>.vim`。
这样启动阶段更轻，只有真正打开对应文件类型时，相关设置才会被加载。

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

## 🔧 运维与维护

### 共享入口

对共享映射、报告和整体状态，优先使用这些统一入口：

```vim
:CheckHealth             " 共享健康检查
:VimrcLoadReport         " 模块加载报告
:VimStartupTime          " 启动耗时
```

现在 `CheckHealth` 也会更直观地显示模块信息，包括 `module_id`、是否存在
本地 override、override 的键，以及模块 config 键摘要。

修改 `config/mapping/*.vim`、`core/keymap.vim` 或 which-key 注册表行为后，
请执行：

```powershell
pwsh -File vimrc/scripts/verify_keymaps.ps1
```

这个脚本会重新生成 `docs/keymaps.md`，并检查编码与格式。

### 插件级状态

调试具体插件时，再使用插件原生命令。
常见例子有 `:CocInfo`、`:Vista info`、`:Clap files`，但实际可用命令取决于
当前启用的模块。

---

## 🐛 故障排除

1. 先运行 `:CheckHealth` 和 `:VimrcLoadReport`，区分依赖问题和模块加载问题。
2. 如果是快捷键文档或注册表漂移，重新执行
   `pwsh -File vimrc/scripts/verify_keymaps.ps1`。
3. 如果是 CoC、Vista、Clap 相关问题，先确认 `node`、`rg`、`fd`、`ctags`
   等外部工具可用，再查看 `:CocInfo`、`:Vista info` 等插件状态命令。

---

## 📚 扩展配置

### 添加插件

```bash
cd pack/mcge/start
git clone https://github.com/author/plugin-name
```

添加插件后，优先从 `modules/module.template.vim` 复制一份，再按
[ARCHITECTURE.md](./ARCHITECTURE.md) 的目录职责，把行为接到 `modules/`、
`config/` 或 `after/ftplugin/`。

### 添加 CoC 扩展

```vim
:CocInstall coc-extension-name
```

### 检查模块状态

可以通过共享健康检查入口，以及部分模块暴露的辅助函数查看状态，例如：

```vim
:CheckHealth
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
