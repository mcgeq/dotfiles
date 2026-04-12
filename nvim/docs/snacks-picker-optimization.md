# snacks picker 当前配置说明

## 当前位置

当前 `snacks` 相关配置已经迁移到新结构，主要文件是：

- `nvim/lua/plugins/snacks.lua`

这份文档只描述当前真正生效的 picker 配置，不再对应旧的 `lua/plugins/snacks.nvim.lua`。

## 当前目标

这套 picker 配置的方向是：

- 用 `snacks.picker` 作为主力查找入口
- 保持默认布局足够通用
- 打开速度快
- 常用文件排序更合理
- 快捷键统一到当前新结构

## 当前生效配置

当前 picker 的核心配置如下：

```lua
picker = {
  matcher = {
    frecency = true,
    case_mode = "smart_case",
  },
  layout = {
    preset = "default",
  },
  formatters = {
    file = {
      filename_first = true,
    },
  },
  files = {
    hidden = false,
    follow = true,
    exclude = {
      ".git",
      "node_modules",
      "dist",
      "build",
      "target",
      ".next",
      ".cache",
    },
  },
  win = {
    input = {
      keys = {
        ["<Esc>"] = { "close", mode = { "n", "i" } },
        ["<C-e>"] = { "list_down", mode = { "n", "i" } },
        ["<C-u>"] = { "list_up", mode = { "n", "i" } },
      },
    },
  },
}
```

## 这和旧说明的差别

下面这些旧文档里的说法，已经不是当前配置：

- 不再写 `throttle = 20`
- 当前布局不是 `ivy`，而是 `default`
- 当前没有把 `<C-j>/<C-k>/<C-n>/<C-p>` 作为 picker 输入区主按键
- 当前文档也不再假设存在旧的 `projects` 独立优化配置文件

也就是说，旧文档更像一份“曾经的优化设想”，现在这份才是实际运行状态。

## 当前体验特点

### 1. `frecency`

开启了 `frecency = true`，常用且最近访问过的文件会更靠前。

### 2. `smart_case`

开启了 `case_mode = "smart_case"`：

- 小写搜索时更宽松
- 带大写时更偏精确匹配

### 3. 文件名优先显示

`filename_first = true` 会先展示文件名，再展示路径，扫结果更快。

### 4. 默认启用预览

当前使用默认 layout，文件类 picker 会按 `snacks` 当前版本的默认行为显示预览窗口，方便在不真正打开文件的情况下确认目标。

### 5. 避开大目录

默认排除：

- `.git`
- `node_modules`
- `dist`
- `build`
- `target`
- `.next`
- `.cache`

对前端、Rust、全栈项目都更实用。

## 当前常用快捷键

这些映射来自：

- `nvim/lua/plugins/snacks.lua`

当前主力查找相关快捷键：

- `<leader><space>`: 智能查找
- `<leader>,`: buffer 列表
- `<leader>/`: 全局 grep
- `<leader>ff`: 查找文件
- `<leader>fg`: 查找 git 文件
- `<leader>fr`: 最近文件
- `<leader>fc`: 查找 Neovim 配置文件
- `<leader>sb`: 当前 buffer 行内搜索
- `<leader>sw`: 搜索当前单词
- `<leader>sd`: 项目诊断
- `<leader>sD`: 当前 buffer 诊断
- `<leader>sk`: 快捷键列表
- `<leader>sh`: help
- `<leader>sn`: 通知历史
- `<leader>sR`: 恢复上一次 picker
- `<leader>ss`: 文档符号
- `<leader>sS`: 工作区符号
- `<leader>sm`: marks
- `<leader>sc`: commands
- `<leader>sp`: 插件目录文件

Git 工作流相关：

- `<leader>gb`: git branches
- `<leader>gg`: lazygit
- `<leader>gs`: git status
- `<leader>gl`: git log
- `<leader>gf`: 当前文件 git log
- `<leader>gL`: 当前行 git log

LSP 跳转也走了 `snacks.picker`：

- `gd`: definition
- `gD`: declaration
- `gr`: references
- `gI`: implementation
- `gy`: type definition

## picker 内部按键

当前额外定义的输入区按键是：

- `<Esc>`: 关闭 picker
- `<C-e>`: 向下选择
- `<C-u>`: 向上选择

这部分是为了尽量统一当前配置的操作习惯，而不是继续沿用旧文档里的那一套组合键。

## 自定义入口

如果你后面要覆盖 picker 行为，建议不要直接改核心逻辑，优先放到用户层：

- `nvim/lua/user/plugins/`

适合放在用户层覆盖的内容：

- 默认布局
- 是否显示隐藏文件
- 排除目录
- picker 内部按键
- 额外的搜索快捷键

## 结论

当前这套 `snacks picker` 已经不是“旧 AstroNvim 配套优化”，而是新配置体系里的主搜索入口。后续如果继续调优，建议以 `nvim/lua/plugins/snacks.lua` 和 `lua/user/plugins/` 为准，而不是再参考旧路径。
