# CoC 安装优化指南

## 已应用的优化

### 1. 使用国内 npm 镜像

在 `config/coc-settings.json` 中已配置：
```json
{
  "npmRegistry": "https://registry.npmmirror.com",
  "npmClient": "npm"
}
```

### 2. 批量安装扩展

修改了 `modules/lsp/coc.vim` 中的自动安装逻辑：
- 每次批量安装 5 个扩展（而不是逐个安装）
- 显示安装进度
- 减少重复检测

---

## 手动优化方案

### 方案一：首次启动时手动批量安装（推荐）

```vim
" 在 Vim 中执行
:CocInstall coc-clangd coc-rust-analyzer coc-tsserver coc-java coc-lua coc-vimlsp coc-zig coc-sh coc-clojure
:CocInstall coc-html coc-css coc-cssmodules coc-eslint coc-prettier @yaegassy/coc-tailwindcss3 coc-unocss coc-vetur @yaegassy/coc-volar
:CocInstall coc-json coc-yaml coc-xml coc-toml
:CocInstall coc-explorer coc-git coc-highlight coc-lightbulb coc-lists coc-pairs coc-snippets coc-yank coc-tabnine coc-webview
:CocInstall coc-cmake coc-sql coc-markdownlint coc-markdown-preview-enhanced
```

说明：
- 前端格式化默认走 `coc-eslint`，`prettier.enable` 默认为 `false`。
- `coc-prettier` 仍可保留安装，方便在个别项目里手动开启 Prettier 配置入口。
- 保存时的自动格式化当前只对前端文件类型启用：
  `javascript`、`javascriptreact`、`typescript`、`typescriptreact`、`vue`。
  其中 `jsx` 对应 `javascriptreact`，`tsx` 对应 `typescriptreact`。

### 方案二：禁用自动安装，改为手动安装

编辑 `modules/lsp/coc.vim`，将：
```vim
auto_install: true,
```
改为：
```vim
auto_install: false,
```

然后在首次启动时手动执行：
```vim
:CocInstall coc-clangd coc-rust-analyzer coc-tsserver
```

### 方案三：使用 yarn 代替 npm（更快）

1. 安装 yarn：
```powershell
npm install -g yarn
```

2. 编辑 `config/coc-settings.json`：
```json
{
  "npmClient": "yarn"
}
```

---

## 网络优化

### Windows 用户

创建/编辑 `%APPDATA%\coc\settings.json`：
```json
{
  "npmRegistry": "https://registry.npmmirror.com",
  "http.proxy": "",
  "https.proxy": ""
}
```

### Linux/macOS 用户

创建/编辑 `~/.config/coc/settings.json`：
```json
{
  "npmRegistry": "https://registry.npmmirror.com",
  "http.proxy": "",
  "https.proxy": ""
}
```

---

## 预安装扩展（可选）

有些扩展可能不需要，可以从 `modules/lsp/coc.vim` 中移除：

```vim
# 不常用的扩展（可以注释掉）
# "coc-clojure",           # Clojure
# "coc-java",              # Java
# "coc-zig",               # Zig
# "coc-sql",               # SQL
# "coc-cmake",             # CMake
```

---

## 查看安装进度

```vim
:CocList extensions          " 查看已安装扩展
:CocInfo                     " 查看 CoC 状态
:CocCommand extensions.manage  " 管理扩展
```

---

## 常见问题

### Q: 某个扩展一直安装失败？
```vim
:CocUninstall coc-extension-name
:CocInstall coc-extension-name
```

### Q: 想查看安装日志？
```vim
:CocInfo
```
然后查看 output 窗口中的 CoC 日志。

### Q: 安装后扩展不工作？
```vim
:CocRestart
```

---

## 预计安装时间

| 网络环境 | 扩展数量 | 预计时间 |
|---------|---------|---------|
| 国内镜像 | 45 个 | 5-10 分钟 |
| 官方源 | 45 个 | 20-40 分钟 |
| 官方源（慢速网络）| 45 个 | 1 小时+ |

**建议使用国内镜像！**
