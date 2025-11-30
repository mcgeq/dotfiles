# Local 用户自定义配置

此目录用于存放你的个人配置，这些文件不会被 Git 跟踪。

## 📁 文件说明

### `user_settings.vim`
- 用于覆盖默认配置
- 设置个人偏好的选项
- 监听配置加载事件

### `user_mappings.vim`（可选）
- 自定义按键映射
- 不会影响默认映射

### `user_plugins.vim`（可选）
- 添加额外的插件
- 个人专用的插件配置

## 🚀 快速开始

1. **复制示例文件：**
   ```bash
   cd d:\config\dotfiles\vimrc\local
   copy user_settings.example.vim user_settings.vim
   ```

2. **编辑配置：**
   ```vim
   :e d:\config\dotfiles\vimrc\local\user_settings.vim
   ```

3. **重新加载：**
   ```vim
   :source $MYVIMRC
   ```

## 📝 示例

### 自定义欢迎信息

已在 `user_settings.vim` 中配置，会在 Vim 启动完成后显示：
- ✓ 配置版本
- ⚡ 启动时间
- 📦 模块加载统计
- 💡 快捷键提示

### 覆盖默认设置

```vim
# 在 user_settings.vim 中添加
set number          # 显示行号
set cursorline      # 高亮当前行
colorscheme desert  # 更改配色方案
```

### 自定义快捷键

```vim
# 在 user_settings.vim 中添加
nnoremap <leader>w :w<CR>
nnoremap <leader>q :q<CR>
```

## 💡 注意事项

- ✅ 这些文件会在配置加载的最后阶段加载
- ✅ 可以覆盖任何默认设置
- ✅ 不会被 Git 跟踪，适合存放个人配置
- ✅ 删除这些文件不会影响 Vim 正常运行

## 🔗 相关文档

- **README_REFACTOR.md** - 重构版配置说明
- **QUICK_START.md** - 快速开始指南
- **MIGRATION_GUIDE.md** - 迁移指南
