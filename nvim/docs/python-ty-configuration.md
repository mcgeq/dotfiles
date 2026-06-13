# ty 配置说明

## 当前位置

当前这套 Neovim 配置里，`ty` 的定义已经不在旧的 AstroNvim 插件文件中，而是在：

- `nvim/lua/lsp/servers.lua`

对应的是默认 Python 类型检查 / LSP 主线。当前策略是：

- 系统存在 `ty` 时，优先使用 `uv + ty`
- 系统没有安装 `ty` 时，自动退回 `basedpyright`
- `ruff` 保持负责 lint / fix / format，不承担主类型服务职责

## 当前配置

当前实际生效的 `ty` 配置大致如下：

```lua
local ty_cmd = python_ty_cmd()
local use_ty = ty_cmd ~= nil

basedpyright = {
  name = "basedpyright",
  ensure_installed = not use_ty,
  enable = not use_ty,
  config = {
    root_markers = PYTHON_ROOT_MARKERS,
    settings = {
      basedpyright = {
        analysis = {
          typeCheckingMode = "standard",
        },
      },
    },
  },
}

ty = {
  name = "ty",
  ensure_installed = false,
  enable = use_ty,
  config = {
    cmd = ty_cmd or { "ty", "server" },
    filetypes = { "python" },
    root_markers = {
      "pyproject.toml",
      "uv.lock",
      "ruff.toml",
      ".ruff.toml",
      "requirements.txt",
      ".git",
    },
  },
}
```

这表示：

- 默认不会由 Mason 自动安装 `ty`，因为推荐用系统工具链 / `uv` 管理
- 当 `ty` 可执行文件存在时，Neovim 会优先执行 `uv run ty server`
- 如果没有 `uv` 但有 `ty`，会退为直接执行 `ty server`
- 如果系统没有 `ty`，当前配置会启用 `basedpyright` 作为第二选择
- Python LSP 只在 Python 项目中尝试启动
- 项目根目录会优先根据 `pyproject.toml`、`uv.lock`、`ruff` 配置和 `.git` 判断

## 为什么默认使用 ty

当前配置里，Python 生态默认主线是：

- `ruff` 负责 lint / import / 一部分代码质量能力
- `ty` 负责类型检查和语言服务
- `basedpyright` 只作为没有 `ty` 时的备用 LSP

这样做的好处是：

- 启动更轻
- 适合 `uv` 工作流
- Python 主线更统一，不会同时启动多个完整类型检查器

## 项目侧建议

如果要让 `ty` 在项目里发挥作用，建议项目根目录至少具备以下之一：

- `pyproject.toml`
- `uv.lock`
- `requirements.txt`

更推荐使用 `pyproject.toml + uv`。

一个常见示例：

```toml
[tool.ty]
pythonVersion = "3.12"
include = ["src", "tests"]
exclude = [".venv", "build", "dist"]
typeCheckingMode = "standard"
```

如果你希望更严格，也可以改成：

```toml
[tool.ty]
pythonVersion = "3.12"
include = ["src", "tests"]
typeCheckingMode = "strict"
```

## 命令行检查

当前 Neovim 配置假设你平时也可以直接这样使用 `ty`：

```bash
uv run ty check .
uv run ty server
```

如果这两个命令能正常工作，Neovim 侧接入通常也不会有问题。

## 与当前配置的关系

这份文档对应的是“当前新结构”：

- LSP 定义：`nvim/lua/lsp/servers.lua`
- 用户扩展入口：`nvim/lua/user/lsp/`
- 用户语言扩展入口：`nvim/lua/user/lang.lua`

如果你后面想覆盖默认行为，不需要改核心文件，优先放到 `lua/user/` 下。

## 自定义建议

如果你想调整 Python 类型检查器，可以在用户层做两类调整：

- 覆盖 `ty` 或 `basedpyright` 的参数
- 按项目选择是否保留 `ruff` 的部分职责

建议不要直接回到旧的 `lua/plugins/python-ty-ruff.lua` 结构，那一套已经不再是当前配置体系的一部分。
