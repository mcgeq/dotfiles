# ty 配置说明

## 当前位置

当前这套 Neovim 配置里，`ty` 的定义已经不在旧的 AstroNvim 插件文件中，而是在：

- `nvim/lua/lsp/servers.lua`

对应的是一个“可选启用”的 Python LSP / 类型检查服务。

## 当前配置

当前实际生效的 `ty` 配置大致如下：

```lua
ty = {
  name = "ty",
  ensure_installed = false,
  config = {
    cmd = { "uv", "run", "ty", "server" },
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

- 默认不会由 Mason 自动安装 `ty`
- 需要本机能执行 `uv run ty server`
- 只在 Python 项目中尝试启动
- 项目根目录会优先根据 `pyproject.toml`、`uv.lock`、`ruff` 配置和 `.git` 判断

## 为什么默认不强制启用

当前配置里，Python 生态默认主线更偏向：

- `ruff` 负责 lint / import / 一部分代码质量能力
- 你可以按项目需要再接入 `ty`

这样做的好处是：

- 启动更轻
- 不会强制所有 Python 项目都依赖 `ty`
- 适合 `uv` 工作流

如果你当前项目本来就使用 `ty`，这份配置已经预留好了接入口。

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

如果你想把 `ty` 作为主力 Python 类型检查器，可以在用户层做两类调整：

- 显式启用 / 覆盖 `ty` 的参数
- 按项目选择是否保留 `ruff` 的部分职责

建议不要直接回到旧的 `lua/plugins/python-ty-ruff.lua` 结构，那一套已经不再是当前配置体系的一部分。
