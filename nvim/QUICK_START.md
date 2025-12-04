# ⚡ 快速启用优化功能

## 1️⃣ 启用用户命令（1分钟）

在 `lua/polish.lua` 末尾添加：

```lua
-- 启用配置管理命令
require("config.commands").setup()
```

**测试**：
```vim
:PresetList      " 查看可用预设
:ConfigInfo      " 查看配置信息
:KeymapDocs      " 查看键位映射
```

---

## 2️⃣ 启用配置验证（2分钟）

在 `lua/polish.lua` 添加：

```lua
-- 启动时自动验证配置
require("config.validator").auto_validate()
```

**测试**：
重启 Neovim，会自动检查配置并显示警告（如有）

---

## 3️⃣ 使用键位管理器（5分钟）

编辑 `lua/plugins/astrocore.lua`，找到 `mappings` 部分：

**Before**:
```lua
opts.mappings = {
  n = {
    ["<C-s>"] = { ":w!<CR>", desc = "Save File" },
    -- ...更多键位...
  },
}
```

**After**:
```lua
-- 使用键位管理器
opts.mappings = require("config.keymaps").build_mappings()

-- 如需添加额外键位，在后面覆盖
opts.mappings.n["<Leader>x"] = { ":CustomCmd<CR>", desc = "Custom" }
```

**测试**：
```vim
:KeymapDocs  " 查看所有键位
```

---

## 4️⃣ 尝试预设系统（10分钟）

### 方法 1：使用命令
```vim
:PresetList              " 查看所有预设
:PresetSwitch minimal    " 切换到最小化配置
" 重启 Neovim
:ConfigInfo              " 确认已切换
```

### 方法 2：环境变量
```bash
# Linux/macOS
export NVIM_PRESET=frontend
nvim

# Windows PowerShell
$env:NVIM_PRESET="frontend"
nvim
```

### 方法 3：配置文件
```bash
# 在 nvim 配置目录创建 .preset 文件
echo "frontend" > ~/.config/nvim/.preset
# Windows
echo frontend > ~\AppData\Local\nvim\.preset
```

**测试**：
```vim
:PresetList    " 查看当前预设（带 → 标记）
:PluginStats   " 查看插件数量变化
```

---

## 5️⃣ 完全迁移（可选，30分钟）

### 备份现有配置
```bash
cd ~/.config/nvim
git add -A
git commit -m "backup: before optimization migration"
```

### 切换到新配置系统

编辑 `lua/lazy_setup.lua`：

**Before**:
```lua
{ import = "community" },
```

**After**:
```lua
{ import = "community_v2" },
```

### 测试
```vim
:Lazy sync           " 同步插件
:ConfigValidate      " 验证配置
:PluginStats         " 查看统计
```

### 回退（如有问题）
```bash
git reset --hard HEAD~1
```

---

## 📋 验证清单

启用每个功能后，运行以下命令验证：

- [ ] `:PresetList` - 显示预设列表
- [ ] `:ConfigInfo` - 显示配置信息
- [ ] `:KeymapDocs` - 显示键位文档
- [ ] `:ConfigValidate` - 无错误或警告
- [ ] `:PluginStats` - 显示正确的统计

---

## 🆘 问题排查

### 命令不存在
```vim
:lua require("config.commands").setup()  " 手动注册命令
```

### 模块加载失败
```vim
:lua print(vim.fn.stdpath("config"))  " 检查配置路径
:lua print(vim.inspect(package.loaded))  " 检查已加载模块
```

### 预设不生效
```bash
# 检查预设文件
cat ~/.config/nvim/.preset  # Linux/macOS
type ~\AppData\Local\nvim\.preset  # Windows

# 删除预设文件，使用默认
rm ~/.config/nvim/.preset
```

---

## 💡 推荐流程

**第1天**：启用命令和验证器，熟悉新功能  
**第2-3天**：使用键位管理器，整理现有键位  
**第4-7天**：尝试不同预设，找到最适合的  
**第2周**：考虑完全迁移到 `community_v2.lua`

**关键提示**：每一步都先测试，确认无误后再继续！

---

## 📚 更多信息

- 详细指南：`OPTIMIZATION_GUIDE.md`
- 优化报告：`OPTIMIZATION_REPORT.md`
- 配置文档：`README.md`

祝你使用愉快！ 🚀
