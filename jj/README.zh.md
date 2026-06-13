[English](README.md)

# Jujutsu 配置

Jujutsu（`jj`）版本控制系统配置。包含丰富的 revset 别名、自定义日志模板、Meld 差异工具集成和 Gerrit 支持。

## 特性

- **编辑器**：Neovim
- **差异/合并**：Meld（图形界面）、difft（终端）、mergiraf（快速三路合并）
- **Git 集成**：`.jj`/`.git` 共存、子进程 git
- **Revset 别名**：`open()`、`ready()`、`wip()`、`private()`、`stack()`、`trunk()`、`user()`
- **命令别名**：`s`（show）、`d`（diff）、`ll`（详细日志）、`open`、`ready`、`retrunk`、`reheat`
- **日志模板**：`log1`、`log2`、`log3`、`logv`、`log1current`、`log1author`、`log1nobookmarks`
- **颜色**：自定义节点/Change ID/书签/标签颜色
- **Gerrit**：可选（默认禁用）

## 别名

| 别名 | 命令 | 说明 |
|------|------|------|
| `d` | `diff` | 查看差异 |
| `s` | `show` | 查看提交详情 |
| `ll` | `log -T builtin_log_detailed` | 详细日志 |
| `nt` | `new trunk()` | 从主干创建新变更 |
| `open` | `log -r open()` | 显示打开的提交 |
| `ready` | `log -r ready()` | 显示就绪的提交 |
| `retrunk` | `rebase -d trunk()` | 变基到最新主干 |
| `reheat` | 完整变基工作栈 | 重新变基整个栈 |
| `consume` | `squash --into @ --from` | 压缩到当前提交 |
| `eject` | `squash --from @ --into` | 从当前提交分离 |

详见 [config.toml](config.toml)。
