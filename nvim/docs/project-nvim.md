# project.nvim - 项目管理

## 📖 简介

`project.nvim` 自动检测和管理项目根目录，让你可以在多个项目间快速切换。它会记住最近访问的项目，并自动设置工作目录。

## 🎯 核心功能

- ✅ 自动检测项目根目录（基于 `.git`、`package.json` 等）
- ✅ 记录最近访问的项目
- ✅ 快速切换项目
- ✅ 自动 `cd` 到项目根目录
- ✅ 与 Telescope 集成

## 🔍 项目检测规则

插件通过以下文件/目录识别项目根目录：

```
.git/
.gitignore
Makefile
package.json
go.mod
Cargo.toml
pyproject.toml
requirements.txt
pom.xml
build.gradle
CMakeLists.txt
```

## ⌨️ 快捷键

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `<leader>fp` | 打开项目列表 | 选择最近访问的项目 |
| `<leader>fP` | 搜索项目文件 | 在所有项目中搜索 |

### Telescope 项目面板

打开项目列表后（`<leader>fp`）：

| 快捷键 | 功能 |
|--------|------|
| `<CR>` | 切换到选中项目 |
| `<C-d>` | 从列表删除项目 |
| `<C-v>` | 在垂直分割窗口打开 |
| `<C-s>` | 在水平分割窗口打开 |
| `<C-t>` | 在新标签页打开 |

## 🔥 使用场景

### 1. 快速切换项目

```
# 场景：你正在开发多个项目
~/projects/frontend/  # 前端项目
~/projects/backend/   # 后端项目
~/projects/docs/      # 文档项目

# 操作
<leader>fp  # 打开项目列表
# 使用 j/k 选择项目
<CR>        # 切换到选中项目
```

### 2. 自动设置工作目录

```
# 打开项目中的任意文件
nvim ~/projects/myapp/src/components/Header.tsx

# project.nvim 自动将工作目录设置为
# ~/projects/myapp/  (项目根目录)

# 验证：:pwd
# 输出：/home/user/projects/myapp
```

### 3. 跨项目搜索文件

```
<leader>fp   # 选择项目
<leader>ff   # 在该项目中搜索文件
```

### 4. 管理最近项目

```
# 项目会自动添加到历史记录
# 访问过的项目都会出现在 <leader>fp 列表中
```

## 💡 实用技巧

### 1. 工作流程

```bash
# 1. 打开任意项目文件
nvim ~/work/project-a/src/main.ts

# 2. 需要切换到另一个项目
<leader>fp  # 打开项目列表

# 3. 选择 project-b
<CR>

# 4. 现在工作目录是 ~/work/project-b/
# 可以使用 <leader>ff 搜索该项目的文件
```

### 2. 结合 Telescope

```vim
" 在当前项目中搜索文件
<leader>ff  " Find files

" 在当前项目中搜索文本
<leader>fw  " Find words (live grep)

" 所有操作都限定在项目根目录内
```

### 3. 手动添加项目

如果项目没有被自动检测，可以手动添加：

```vim
:ProjectRoot  " 手动设置当前目录为项目根
```

### 4. 移除项目

```
<leader>fp  # 打开项目列表
<C-d>       # 删除选中项目（不会删除文件，仅从列表移除）
```

## 🎨 工作流示例

### 全栈开发工作流

```
项目结构：
~/dev/
  ├── frontend/  (.git, package.json)
  ├── backend/   (.git, go.mod)
  └── mobile/    (.git, pubspec.yaml)

# 工作流程
1. nvim ~/dev/frontend/src/App.tsx
   # 自动 cd 到 ~/dev/frontend/

2. 需要修改后端
   <leader>fp -> 选择 backend -> <CR>
   # 自动 cd 到 ~/dev/backend/

3. 搜索后端文件
   <leader>ff -> 输入文件名
   # 只在 backend 项目中搜索

4. 需要查看 API 文档
   <leader>fp -> 选择 docs -> <CR>
   # 自动 cd 到文档项目
```

### 微服务项目管理

```
~/microservices/
  ├── user-service/
  ├── order-service/
  ├── payment-service/
  └── notification-service/

# 快速在各个服务间切换
<leader>fp  # 所有服务都会列出
# 选择需要的服务，立即切换上下文
```

## ⚙️ 配置说明

### 自定义检测模式

```lua
{
  "ahmedkhalf/project.nvim",
  opts = {
    detection_methods = { "pattern", "lsp" },
    patterns = {
      ".git",
      "package.json",
      "Makefile",
      -- 添加自定义模式
      ".project",  -- 自定义项目标记文件
      "*.sln",     -- Visual Studio 解决方案
    }
  }
}
```

### 排除路径

```lua
{
  "ahmedkhalf/project.nvim",
  opts = {
    exclude_dirs = {
      "~/.cargo/*",
      "~/.local/*",
      "*/node_modules/*",
    }
  }
}
```

### 自动切换目录

```lua
{
  "ahmedkhalf/project.nvim",
  opts = {
    silent_chdir = false,  -- 切换时显示消息
    manual_mode = false,   -- 自动模式（推荐）
  }
}
```

## 📚 集成说明

### 与 Telescope 集成

```lua
-- 在 Telescope 中使用项目管理器
require('telescope').load_extension('projects')

-- 使用命令
:Telescope projects
```

### 与 Neo-tree 集成

切换项目后，Neo-tree 会自动更新文件树到新项目根目录。

### 与 LSP 集成

LSP 服务器会在项目根目录启动，切换项目后会重启相应的 LSP。

## 🆚 对比手动管理

| 场景 | 手动管理 | project.nvim |
|------|---------|--------------|
| 切换项目 | `:cd ~/path/to/project` | `<leader>fp` + `<CR>` |
| 查找根目录 | 手动向上查找 `.git` | 自动检测 |
| 记住项目 | 手动记录路径 | 自动历史记录 |
| 文件搜索 | 需要指定路径 | 自动限定项目范围 |

## 🔗 相关资源

- [GitHub - project.nvim](https://github.com/ahmedkhalf/project.nvim)
- [AstroCommunity 插件页](https://github.com/AstroNvim/astrocommunity/tree/main/lua/astrocommunity/project/project-nvim)
- [Telescope 集成文档](https://github.com/nvim-telescope/telescope.nvim)
