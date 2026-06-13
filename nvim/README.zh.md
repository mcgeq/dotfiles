# Neovim 配置

此目录为当前活跃的 Neovim 配置文件。基于 Neovim 0.12+ 原生特性和 `vim.pack` 重建，通过 `lua/user/` 提供稳定的覆盖层。

旧的 AstroNvim / Lazy / 预设结构不再作为事实来源。如需扩展或覆盖行为，请参阅 [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md)。

历史文档如 `MIGRATION.md`、`LEGACY-FILES.md`、`ENHANCEMENT_GUIDE.md` 仅为归档用途。本 README 中的文件为当前事实来源。

## 维护契约

- [README.md](/d:/config/dotfiles/nvim/README.md) 描述当前行为、支持的命令和扩展模型
- [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md) 说明个性化修改的存放位置
- [docs/architecture.md](/d:/config/dotfiles/nvim/docs/architecture.md) 描述模块所有权和运行流程
- `MIGRATION.md`、`LEGACY-FILES.md`、`ENHANCEMENT_GUIDE.md` 为迁移历史的归档材料

## 设计目标

- 优先使用 Neovim 内置功能
- 在现代插件能显著提升体验时保留它们
- 保持快捷键可预测、按组组织
- 保持个性化覆盖与基础配置隔离
- 支持前端、后端、Markdown 和终端密集型工作流，但不引入框架层

## 当前技术栈

核心：

- 原生 `vim.pack` 插件管理
- 原生 `vim.lsp.config()` 和 `vim.lsp.enable()`
- Mason 用于工具和 LSP 安装
- Treesitter 用于语法和结构解析
- Conform 用于非前端格式化
- 重型可选插件通过 `vim.pack` 安装，部分工作流工具按需通过 `:packadd` 加载

UI 和导航：

- `snacks.nvim`：仪表盘、选择器、文件浏览器、通知、Git 搜索
- `flash.nvim`：增强光标跳转
- `nvim-spider`：更智能的 `w/e/b/ge` 单词移动
- `noice.nvim`：消息和命令行 UI
- `blink.cmp`：代码补全
- `bufferline.nvim`：缓冲区标签页
- `which-key.nvim`：分组快捷键提示
- `nvim-spectre`：交互式搜索替换
- `trouble.nvim`：诊断、符号、quickfix 和 location 列表
- `todo-comments.nvim`：TODO / FIXME 导航
- `nvim-treesitter-context`：轻量级代码上下文

主题：

- `tokyonight`
- `catppuccin`
- `kanagawa`

如果首选主题缺失或损坏，配置会自动回退到磁盘上实际存在的主题。

## 语言支持

LSP 直接在 [lua/lsp/servers.lua](/d:/config/dotfiles/nvim/lua/lsp/servers.lua) 中配置。

内置基础支持包括：

- C / C++：通过 [lua/user/lsp/clangd.lua](/d:/config/dotfiles/nvim/lua/user/lsp/clangd.lua) 配置 `clangd`
- Lua：`lua_ls`
- Bash：`bashls`
- JSON：`jsonls` + SchemaStore 模式
- YAML：`yamlls` + SchemaStore 模式
- TOML：`taplo`
- Markdown：`marksman`
- Go：`gopls`
- Rust：`rust_analyzer`
- Zig：`zls`
- Python：`ruff` + `uv + ty`（默认），`ty` 未安装时回退到 `basedpyright`
- CSS / HTML：`cssls`、`emmet_language_server`
- JavaScript / TypeScript / React：`vtsls`、`eslint`
- Vue：`vue_ls`，`@vue/typescript-plugin` 接入 `vtsls`

前端格式化默认使用 ESLint 驱动的工作流，以便使用 `@antfu/eslint-config` 的项目保持单一事实来源。非前端文件类型继续使用 Conform。

C++ 用户覆盖优先使用系统 LLVM 工具链，不可用时回退到 Mason 的 `clangd`。C++20/23 默认值、`query-driver` 和项目级模块配置请参阅 [docs/cpp-clangd.md](/d:/config/dotfiles/nvim/docs/cpp-clangd.md)。

## 实用命令

配置和状态：

- `:NvimConfig` 打开配置目录
- `:NvimUserConfig` 打开用户覆盖目录
- `:NvimStatus` 显示当前配置路径、数据路径、主题和前端模式

主题：

- `:Theme` 显示当前主题
- `:Theme <name>` 切换当前会话的主题
- `:ThemeCycle` 轮换内置主题

前端模式：

- `:FrontendMode` 显示当前前端模式
- `:FrontendMode eslint`
- `:FrontendMode eslint_imports`
- `:FrontendMode conform`
- `:FrontendModeCycle`

插件管理：

- `:PackUpdate` 打开模糊选择器选择插件更新
- `:PackUpdate <plugin>` 更新指定插件
- `:PackUpdate!` 更新全部插件
- `:PackStatus` 检查插件状态
- `:PackRepair` 显示损坏或缺失的插件
- `:PackRepair <plugin>` 重新安装指定损坏插件
- `:PackRepair!` 修复所有损坏或缺失的插件
- `:PackSync` 将当前解析状态写入 [nvim-pack-lock.json](/d:/config/dotfiles/nvim/nvim-pack-lock.json)
- `:PackOpenLog` 打开轻量级 `vim.pack` 活动日志
- `:MarkdownTableFormat` 格式化光标下的 Markdown 表格
- `:RenderMarkdown toggle` 切换缓冲区 Markdown 渲染
- `:RenderMarkdown preview` 在侧边打开渲染后的 Markdown 预览

终端：

- `:TerminalCwd` 在当前工作目录打开浮动终端
- `:TerminalProject` 在当前项目根目录打开浮动终端
- `:TerminalBufferDir` 在当前缓冲区目录打开浮动终端
- `:TerminalLast` 重新打开上一个浮动终端命令

工作区会话：

- `:SessionInfo` 显示当前工作区的会话目标
- `:SessionSave` 保存当前工作区会话
- `:SessionLoad` 加载当前工作区会话
- `:SessionSelect` 选择并加载已保存的工作区会话
- `:SessionDelete` 删除当前工作区会话
- `:SessionSelectDelete` 选择并删除已保存的工作区会话
- `:SessionRestart` 重启 Neovim 并恢复当前工作区会话

项目和根目录：

- `:ProjectInfo` 显示 cwd、检测到的项目根目录、当前缓冲区根目录和会话状态
- `:ProjectRoot` 将当前工作目录设置为当前缓冲区的项目根目录
- `:ProjectSelect` 选择已知项目并切换 cwd
- `:ProjectSessionSelect` 选择已知项目、切换 cwd 并在存在时加载其保存的会话

运行器：

- `:RunnerInfo` 显示当前缓冲区的运行/测试操作
- `:RunnerBuild` 构建当前项目或缓冲区目标
- `:RunnerRun` 运行当前项目或缓冲区目标
- `:RunnerTest` 运行当前项目或缓冲区测试目标
- `:RunnerTestFile` 运行当前文件的测试（如支持）
- `:RunnerRepeat` 重复上一个运行器命令
- `:RunnerClean` 清理当前项目或缓冲区目标

C++ / CMake 扩展：

- `clangd_extensions.nvim` 提供源/头文件切换及 AST、符号、层次结构和内存视图
- `cmake-tools.nvim` 为 CMake 项目提供编辑器内生成/构建/运行工作流
- `nvim-dap`、`dap-ui` 和 `dap-virtual-text` 为 Python 及 C/C++/Rust/Zig 提供调试基础（通过 `debugpy` 和 `codelldb`）

当插件目录损坏且仅剩 `.git` 文件夹时，`PackRepair` 尤其有用。

## 快捷键一览

全局：

- `<leader><space>` 智能文件选择
- `<leader>,` 缓冲区列表
- `<leader>/` 项目全局搜索
- `<leader>e` 文件浏览器
- `<C-s>` 保存
- `<leader>qq` 退出所有

缓冲区：

- `<leader>bb` 选择缓冲区
- `<leader><Tab>` 切换到上一个缓冲区
- `<leader>bd` 删除当前缓冲区
- `<leader>bo` 删除其他缓冲区
- `<leader>bP` 删除当前项目中的其他缓冲区
- `<leader>bh` 删除左侧缓冲区
- `<leader>bl` 删除右侧缓冲区
- `<leader>bn` 下一个缓冲区
- `<leader>bp` 上一个缓冲区
- `<leader>bt` 切换当前缓冲区固定状态
- `<leader>bH` 将当前缓冲区左移
- `<leader>bL` 将当前缓冲区右移
- `<leader>b1` ... `<leader>b9` 按位置跳转到缓冲区

窗口：

- `<C-h/j/k/l>` 按方向切换窗口焦点
- `<leader>wm` 最大化或恢复当前窗口
- `<leader>wo` 关闭其他窗口
- `<leader>wv` 垂直分割
- `<leader>ws` 水平分割
- `<leader>wq` 关闭窗口
- `<leader>wT` 将当前窗口移至新标签页
- `<leader>w=` 平衡窗口大小

标签页：

- `<leader>tn` 新建标签页
- `<leader>th` 上一个标签页
- `<leader>tl` 下一个标签页
- `<leader>tq` 关闭当前标签页
- `<leader>to` 关闭其他标签页

编辑：

- `<A-j>` / `<A-k>` 向下或向上移动当前行或选中的内容
- `<leader>od` 向下复制当前行
- 可视模式 `<leader>od` 向下复制选中内容
- `<leader>os` 打开临时缓冲区

列表和跳转：

- `[q` / `]q` 上一个/下一个 quickfix 项
- `[l` / `]l` 上一个/下一个 location 列表项
- `[d` / `]d` 上一个/下一个诊断
- `[e` / `]e` 上一个/下一个错误
- `[w` / `]w` 上一个/下一个警告
- `[t` / `]t` 上一个/下一个 TODO / FIXME 注释

搜索和选择器：

- `<leader>ff` 文件
- `<leader>fg` Git 文件
- `<leader>fr` 最近文件
- `<leader>fc` 配置文件
- `<leader>sb` 缓冲区行搜索
- `<leader>sd` 项目诊断
- `<leader>sD` 缓冲区诊断
- `<leader>ss` LSP 符号
- `<leader>sS` 工作区符号
- `<leader>sm` 标记
- `<leader>sc` 命令
- `<leader>sp` 插件文件
- `<leader>st` TODO / FIXME 注释
- `<leader>sh` 帮助
- `<leader>sk` 快捷键
- `<leader>sn` 通知
- `<leader>sw` 使用 `snacks.picker` 搜索当前单词或选中文本
- `<leader>sr` 使用 `nvim-spectre` 替换文件内容
- `<leader>su` 恢复上一次 `nvim-spectre` 搜索
- `<leader>sQ` 将 `nvim-spectre` 结果发送到 quickfix
- `<leader>sW` 使用 `nvim-spectre` 替换当前单词或选中文本
- `<leader>sF` 使用 `nvim-spectre` 替换当前文件
- `<leader>sR` 恢复上一次选择器

主题和前端运行时切换：

- `<leader>nn` 显示 Neovim 运行时状态
- `<leader>nc` 打开配置目录
- `<leader>nC` 打开用户覆盖目录
- `<leader>nh` 运行异步健康检查
- `<leader>ni` 显示当前缓冲区 LSP 信息
- `<leader>nl` 打开最新的 LSP 日志视图
- `<leader>nL` 打开原始 LSP 日志文件
- `<leader>nR` 重启当前缓冲区的 LSP 客户端
- `<leader>nr` 显示当前文件类型的运行器快捷键
- `<leader>nm` 打开 Mason
- `<leader>nM` 显示 `:messages`
- `<leader>uf` 轮换前端模式
- `<leader>uF` 显示前端模式
- `<leader>ut` 轮换主题
- `<leader>uT` 显示主题
- `<leader>uC` 主题选择器
- `<leader>gg` 通过 `snacks.nvim` 启动 LazyGit
- `<leader>gb` Git 分支
- `<leader>gs` Git 状态
- `<leader>gl` Git 日志
- `<leader>gf` Git 文件日志
- `<leader>gL` Git 行日志

工作区会话：

- `<leader>mi` 显示当前工作区会话目标
- `<leader>ms` 保存当前工作区会话
- `<leader>ml` 加载当前工作区会话
- `<leader>mS` 选择并加载已保存的工作区会话
- `<leader>md` 删除当前工作区会话
- `<leader>mD` 选择并删除已保存的工作区会话
- `<leader>mr` 重启 Neovim 并恢复当前工作区会话

项目和根目录：

- `<leader>Pi` 显示当前项目和根目录信息
- `<leader>Pr` 将 cwd 设置为当前缓冲区的项目根目录
- `<leader>Pp` 选择已知项目
- `<leader>PS` 选择已知项目并自动加载保存的会话

运行器：

- `<leader>ri` 显示当前缓冲区的运行器信息
- `<leader>rb` 构建当前项目或缓冲区目标
- `<leader>rr` 运行当前项目或缓冲区目标
- `<leader>rt` 运行当前项目或包测试
- `<leader>rT` 运行当前文件的测试（如支持）
- `<leader>rl` 重复上一次运行器命令
- `<leader>rc` 清理当前项目或缓冲区目标

工作流和工具：

- `<leader>lt` 在当前工作目录打开终端
- `<leader>lp` 在当前项目根目录打开终端
- `<leader>lb` 在当前缓冲区目录打开终端
- `<leader>lT` 重新打开上一个终端
- `<leader>gj` Jujutsu TUI
- `<leader>js` Jujutsu 状态
- `<leader>jl` Jujutsu 日志
- `<leader>jd` Jujutsu diff
- `<leader>ld` LazyDocker
- `<leader>ls` Live Server
- `<leader>db` 切换 DBUI
- `<leader>df` 查找数据库缓冲区
- `<leader>dr` 重命名数据库缓冲区
- `<leader>dq` 上次查询信息

Git 块：

- `[h` / `]h` 上一个/下一个代码块
- `<leader>ghs` 暂存块
- `<leader>ghr` 重置块
- `<leader>ghp` 预览块
- `<leader>ghb` 代码追溯
- `<leader>ghq` 将当前文件块发送到 quickfix
- `<leader>ghl` 将当前文件块发送到 location 列表
- `<leader>ghS` 暂存当前缓冲区
- `<leader>ghu` 撤销上一次暂存的块
- `<leader>ghD` 比较当前缓冲区差异

调试：

- `<leader>Dc` 继续/启动
- `<leader>Db` 切换断点
- `<leader>DB` 条件断点
- `<leader>Do` 单步跳过
- `<leader>Di` 单步进入
- `<leader>DO` 单步跳出
- `<leader>Dr` 调试 REPL
- `<leader>Dl` 执行上次调试
- `<leader>Dt` 终止
- `<leader>DC` 运行到光标处
- `<leader>Du` 切换 DAP UI

本地运行器（`<localleader>`）：

- `<localleader>rb` 构建
- `<localleader>rr` 运行
- `<localleader>rt` 测试
- `<localleader>rT` 运行当前文件的测试（Python 和部分前端运行器）
- `<localleader>rk` 显示运行器快捷键
- `<localleader>rl` 重复上次运行器命令
- `<localleader>rc` 清理

插件维护：

- `<leader>pu` 选择插件更新
- `<leader>pP` 更新所有插件
- `<leader>ps` 插件状态选择器
- `<leader>pr` 插件修复选择器
- `<leader>pl` 插件日志
- `<leader>pR` 修复待处理的插件
- `<leader>pC` 插件修复并清理
- `<leader>pU` 同步插件锁文件

LSP 和代码导航：

- `K` 悬停文档
- `gd` 跳转到定义
- `gD` 跳转到声明
- `gr` 跳转到引用
- `gI` 跳转到实现
- `gy` 跳转到类型定义
- `<leader>ca` 代码操作
- `<leader>cr` 重命名
- `<leader>xd` 行内诊断
- `<leader>xx` 诊断列表
- `<leader>xX` 缓冲区诊断列表
- `<leader>xq` 诊断发送到 quickfix
- `<leader>xl` 诊断发送到 location 列表
- `<leader>xc` 清空 quickfix 列表
- `<leader>xC` 清空 location 列表
- `<leader>cl` 运行 code lens
- `<leader>cs` 文档符号
- `<leader>xQ` quickfix 列表
- `<leader>xL` location 列表
- `w/e/b/ge` 通过 `nvim-spider` 的智能子词移动

文件类型特定操作使用 `<localleader>`。

示例：

- Markdown：`<localleader>m*`
- `package.json`：`<localleader>n*`
- Hurl：`<localleader>h*`
- 前端缓冲区：`<localleader>l*`、`<localleader>t*`、`<localleader>v*`
- 运行器支持缓冲区：`<localleader>r*`

C / C++（`<localleader>c*`）：

- `<localleader>ck` 显示 C/C++ 快捷键
- `<localleader>cb` 构建文件或 CMake 目标
- `<localleader>cr` 运行文件或 CMake 目标
- `<localleader>cc` 清理文件或 CMake 目标
- `<localleader>ct` 运行测试
- `<localleader>ch` 切换源/头文件
- `<localleader>ca` 显示 AST
- `<localleader>ci` 显示符号信息
- `<localleader>cT` 显示类型层级
- `<localleader>cm` 显示内存使用
- `<localleader>cg` 生成 CMake 项目
- `<localleader>cs` 选择构建目标
- `<localleader>cl` 选择运行目标
- `<localleader>cv` 选择构建类型
- `<localleader>cp` 选择配置预设

Go（`<localleader>`）：

- `<localleader>fs` 填充 struct
- `<localleader>ie` 添加 if err
- `<localleader>at` 添加标签
- `<localleader>aT` 移除标签
- `<localleader>im` 生成实现
- `<localleader>tf` 测试函数
- `<localleader>ta` 测试包
- `<localleader>tc` 测试覆盖率

## 目录结构

```text
nvim/
├── init.lua
├── nvim-pack-lock.json
├── after/
│   └── ftplugin/
├── docs/
├── lua/
│   ├── core/      # 选项、诊断、快捷键、命令、终端辅助、共享工具函数
│   ├── lang/      # 运行时语言模式和前端状态/动作等共享辅助
│   ├── lsp/       # LSP 注册和设置
│   ├── pack/      # vim.pack 配置、命令和 UI
│   ├── plugins/   # 按职责分组的插件设置（编辑/补全/Git/格式化等）
│   └── user/      # 稳定的覆盖入口
```

主要运行链：

1. [init.lua](/d:/config/dotfiles/nvim/init.lua)
2. [lua/core/](/d:/config/dotfiles/nvim/lua/core)
3. [lua/pack/](/d:/config/dotfiles/nvim/lua/pack)
4. [lua/plugins/](/d:/config/dotfiles/nvim/lua/plugins)
5. [lua/lsp/](/d:/config/dotfiles/nvim/lua/lsp)

## 用户覆盖

基础配置保持稳定。个性化修改应放在 [lua/user/](/d:/config/dotfiles/nvim/lua/user) 下。

入口点：

- [lua/user/init.lua](/d:/config/dotfiles/nvim/lua/user/init.lua) 顶层用户覆盖入口，合并以下可选模块
- [lua/user/theme.lua](/d:/config/dotfiles/nvim/lua/user/theme.lua) 主题偏好和高亮覆盖
- [lua/user/pack.lua](/d:/config/dotfiles/nvim/lua/user/pack.lua) 禁用基础插件或添加小型 `vim.pack` 配置
- [lua/user/session.lua](/d:/config/dotfiles/nvim/lua/user/session.lua) 会话行为开关
- [lua/user/lsp_settings.lua](/d:/config/dotfiles/nvim/lua/user/lsp_settings.lua) Mason 支持的 LSP 安装开关
- [lua/user/options.lua](/d:/config/dotfiles/nvim/lua/user/options.lua) 选项覆盖
- [lua/user/keymaps.lua](/d:/config/dotfiles/nvim/lua/user/keymaps.lua) 额外快捷键
- [lua/user/autocmds.lua](/d:/config/dotfiles/nvim/lua/user/autocmds.lua) 额外自动命令
- [lua/user/commands.lua](/d:/config/dotfiles/nvim/lua/user/commands.lua) 自定义命令
- [lua/user/lang.lua](/d:/config/dotfiles/nvim/lua/user/lang.lua) treesitter、格式化器、前端模式默认值、额外 Mason 工具
- [lua/user/plugins/](/d:/config/dotfiles/nvim/lua/user/plugins) 额外插件
- [lua/user/lsp/](/d:/config/dotfiles/nvim/lua/user/lsp) 额外或覆盖的 LSP 服务器
- [after/ftplugin/](/d:/config/dotfiles/nvim/after/ftplugin) 按文件类型的本地行为

示例请参阅 [docs/customization.md](/d:/config/dotfiles/nvim/docs/customization.md)。
重构基础模块前请查阅 [docs/architecture.md](/d:/config/dotfiles/nvim/docs/architecture.md) 了解所有权边界。

## 安装

要求：

- Neovim 0.12 或更新版本
- `git`
- 推荐使用 Nerd Font
- 根据使用语言需要额外工具，例如 `uv`、`ty`、`ruff`、`stylua`、`goimports`、`gofumpt`、`shfmt`、`prettier`、`taplo`、`clang-format`、`debugpy`、`codelldb`

典型安装：

Windows：

```powershell
Move-Item $env:LOCALAPPDATA\nvim $env:LOCALAPPDATA\nvim.bak -ErrorAction SilentlyContinue
git clone <repository-url> $env:LOCALAPPDATA\nvim
nvim
```

Linux / macOS：

```bash
mv ~/.config/nvim ~/.config/nvim.bak 2>/dev/null
git clone <repository-url> ~/.config/nvim
nvim
```

首次启动时，`vim.pack` 会提示安装配置的插件。

## 故障排除

主题加载失败：

- 运行 `:PackStatus` 或 `:PackUpdate`
- 如果插件目录损坏，运行 `:PackRepair <plugin>`
- 插件变更后重启 Neovim

`PackRepair` 提示插件活跃：

- 使用 `NVIM_NO_PLUGINS=1` 重启一次
- 在本次会话中运行 `:PackRepair <plugin>`

无头环境检查在受限沙盒中失败：

- `vim.pack` 引导需要 `git` 在 PATH 中
- Neovim 需要对其数据目录有写权限（日志和 ShaDa）
- 如果在沙盒环境中验证启动，这些错误可能是环境问题而非配置问题

前端保存行为过于激进：

- 使用 `:FrontendMode conform`
- 或调整 [lua/user/lang.lua](/d:/config/dotfiles/nvim/lua/user/lang.lua)

需要快速健康快照：

- `:NvimStatus`
- `:Health`
- `:Health vim.lsp`
- `:messages`

Windows 提示：

- 如 `:checkhealth` 曾经卡死，此配置已包含本地 Mason 健康检查 shim，避免上游 Windows 日志权限问题导致的 `:checkhealth mason` 挂起

## 来自 `cap153-nvim` 的说明

`cap153-nvim` 中的大部分框架级设计已不再沿用。当前配置已吸收了经得起时间考验的部分：

- `snacks.nvim` 作为主要的仪表盘和选择器层
- `blink.cmp`
- `noice.nvim`
- 直接使用 Neovim 0.11+ / 0.12+ LSP 风格而非框架胶水
- 旧版自定义仪表盘标题

未来可能借鉴的候选：

- Neovide 的可选 GUI 专用辅助功能

下步改进方向：

- 将重型可选工具逐步迁移到命令或文件类型触发的按需加载（待 `vim.pack` 的延迟加载模式成熟后）
- 仅当额外适配器对主要语言的维护成本值得时，增加更丰富的测试 UI

已吸收到新基础中的内容：

- Markdown 表格自动格式化（带有手动 `:MarkdownTableFormat` 命令）
- 部分选择器交互细节，如 `<Esc>`、`<C-e>`、`<C-u>`
- `snacks.nvim` 的 lazygit 集成

以上均视为可选增强，而非核心架构。
