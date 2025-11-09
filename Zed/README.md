# Zed 编辑器配置说明

本目录包含 Zed 编辑器的配置文件。

## 配置文件

- `settings.json` - Zed 主配置文件

## 配置说明

**重要提示：** 配置文件已优化为只包含 Zed 支持的属性。如果某些功能不可用，可以通过命令面板运行 `zed: open default settings` 查看所有可用的设置和正确的属性名称。

### Vim 模式
- `vim_mode`: true - 启用 Vim 模式（模态编辑）
  - 启用后将使用 Vim 的按键绑定和编辑模式
  - 可以通过命令面板 `toggle vim mode` 切换

### Vim 配置选项
- `vim.use_system_clipboard`: "always" - 始终使用系统剪贴板
  - 可选值: "always"（始终）、"never"（从不）、"on_yank"（仅在 yank 时）
- `vim.use_smartcase_find`: true - 启用智能大小写搜索
- `vim.toggle_relative_line_numbers`: true - 在普通模式下使用相对行号，插入模式下使用绝对行号
- `vim.highlight_on_yank_duration`: 200 - yank 操作后高亮显示持续时间（毫秒）

### 基础设置
- `ui_font_size`: 16 - UI 字体大小
- `buffer_font_size`: 15 - 编辑器缓冲区字体大小
- `relative_line_numbers`: true - 显示相对行号
- `cursor_blink`: true - 光标闪烁
- `tab_size`: 2 - Tab 键宽度
- `autosave`: "on_focus_change" - 自动保存模式
  - 可选值: "off"（关闭）、"on_focus_change"（焦点改变时）、"on_window_change"（窗口改变时）

### 主题设置
- `theme.mode`: "system" - 跟随系统主题
- `theme.light`: "One Light" - 浅色主题
- `theme.dark`: "One Dark" - 深色主题

## 如何修改配置

1. 直接编辑 `settings.json` 文件
2. 或在 Zed 中使用命令面板（Ctrl+Shift+P / Cmd+Shift+P）输入 `zed: open settings`
3. 查看默认设置：`zed: open default settings`

## 注意事项

- JSON 文件不支持注释，如需说明请参考本 README
- 某些设置可能需要根据 Zed 版本调整
- 如果某个设置不生效，请查看默认设置或移除该设置
- **查看所有可用设置**：在 Zed 中使用命令面板（Ctrl+Shift+P / Cmd+Shift+P），运行 `zed: open default settings` 查看所有可用的配置选项和正确的属性名称
- 当前配置已优化为只包含 Zed 支持的属性，移除了不支持的属性以避免错误
- 如果需要更多功能（如文件排除、格式化等），请查看默认设置以了解正确的属性名称

## Vim 模式功能

启用 vim 模式后，您可以使用标准的 Vim 快捷键：

### 基本操作
- `h/j/k/l` - 移动光标
- `i` - 进入插入模式
- `Esc` - 退出到普通模式
- `v` - 进入可视模式
- `:` - 进入命令模式

### Zed 特有的 Vim 快捷键
- `g d` - 跳转到定义
- `g D` - 跳转到声明
- `g y` - 跳转到类型定义
- `g I` - 跳转到实现
- `c d` - 重命名（更改定义）
- `g A` - 跳转到当前单词的所有引用
- `g s` - 在当前文件中查找符号
- `g S` - 在整个项目中查找符号

### 多光标支持
Vim 模式使用 Zed 的多光标功能实现块选择，使编辑更加灵活。

## 参考文档

- [Zed 配置文档](https://zed.dev/docs/configuring-zed)
- [Zed Vim 模式文档](https://zed.dev/docs/vim)
- [Zed 键位绑定](https://zed.dev/docs/key-bindings)

