# Neovim 新增插件使用文档

本目录包含最近新增的 8 个插件的详细使用说明。

## 📦 插件列表

### 🚀 核心增强（5个）

1. **[mini-ai](./mini-ai.md)** - 增强的文本对象
   - 智能选择函数参数、引号内容、括号块等
   - 关键操作：`ia`/`aa`（参数）、`iq`/`aq`（引号）

2. **[nvim-spider](./nvim-spider.md)** - 智能单词移动
   - 改进 `w`/`e`/`b` 移动，跳过无意义标点
   - 提升 30-50% 导航效率

3. **[nvim-spectre](./nvim-spectre.md)** - 全局搜索和替换
   - 可视化批量搜索替换工具
   - 快捷键：`<leader>sr`（打开）、`<leader>sw`（搜索当前词）

4. **[project.nvim](./project-nvim.md)** - 项目管理
   - 自动检测项目根目录
   - 快捷键：`<leader>fp`（项目列表）

5. **[toggleterm.nvim](./toggleterm-nvim.md)** - 终端集成
   - 浮动终端、多终端管理
   - 快捷键：`<C-\>`（切换终端）、`<leader>gg`（lazygit）

### 💎 进阶工具（3个）

6. **[harpoon](./harpoon.md)** - 文件快速标记和跳转
   - 标记常用文件，数字键快速跳转
   - 快捷键：`<leader>a`（标记）、`<C-h/j/k/l>`（跳转）

7. **[neotest](./neotest.md)** - 测试框架集成
   - 运行和调试测试，查看结果
   - 快捷键：`<leader>tr`（运行测试）、`<leader>td`（调试）

8. **[nvim-toggler](./nvim-toggler.md)** - 文本快速切换
   - 切换 `true/false`、`&&/||` 等对立词
   - 快捷键：`<leader>i`（切换）

### 🎨 核心工具优化

9. **[snacks-picker 优化](./snacks-picker-optimization.md)** - Fuzzy Finder 性能优化
   - Frecency 智能排序、Ivy 布局、预览窗口
   - 大幅提升搜索体验和性能

### 📝 笔记和组织

10. **[Neorg](./neorg.md)** - 现代化笔记系统
    - GTD 任务管理、结构化笔记、知识组织
    - 专为 Neovim 设计的 Org-mode 替代

### 🎨 视觉增强

11. **[Rainbow Delimiter + Indent Blankline](./rainbow-delimiter-indent-blankline.md)** - 彩虹缩进可视化
    - 7色彩虹括号 + 匹配颜色的缩进线
    - 多层嵌套一目了然

### 🪟 窗口管理

12. **[edgy.nvim](./edgy-nvim.md)** - 智能窗口布局管理
    - 自动固定边栏到四个边缘
    - 保持主编辑区域整洁

13. **[colorful-winsep.nvim](./colorful-winsep-nvim.md)** - 彩色窗口分隔符
    - 美化分隔符，当前窗口高亮
    - 平滑动画效果

14. **[windows.nvim](./windows-nvim.md)** - 自动窗口调整
    - 切换窗口时自动扩展
    - 聚焦当前任务

## 🎯 快速上手

### 推荐学习顺序

1. **先掌握核心工具**（1-3天）
   - `nvim-spider` - 立即改善移动体验
   - `toggleterm-nvim` - 日常终端操作
   - `project.nvim` - 多项目管理

2. **进阶文本编辑**（3-5天）
   - `mini-ai` - 学习新的文本对象
   - `nvim-spectre` - 全局重构工具
   - `nvim-toggler` - 快速切换

3. **高级工作流**（1周+）
   - `harpoon` - 优化文件跳转
   - `neotest` - 集成测试流程

## 💡 使用建议

### 日常开发工作流

```
1. 项目管理
   <leader>fp  切换到目标项目
   
2. 文件导航
   <leader>ff  搜索并打开主要文件
   <leader>a   标记常用文件（harpoon）
   <C-h/j/k/l> 快速跳转

3. 代码编辑
   w/e/b       智能移动（spider）
   cia/daa     操作参数（mini-ai）
   <leader>i   切换词（toggler）

4. 搜索替换
   <leader>sr  全局搜索替换（spectre）
   
5. 测试
   <leader>tr  运行测试（neotest）
   
6. 终端操作
   <C-\>       打开终端
   <leader>gg  使用 lazygit
```

## 📚 深入学习

每个插件的文档包含：
- ✅ 详细功能说明
- ✅ 完整快捷键列表
- ✅ 实际使用场景
- ✅ 实用技巧和最佳实践
- ✅ 配置自定义方法

建议按需深入阅读各个插件的详细文档。

## 🔄 安装插件

```vim
" 在 Neovim 中运行
:Lazy sync
```

## 🆘 获取帮助

- 查看 AstroNvim 快捷键：`:Telescope keymaps`
- 查看插件帮助：`:help <插件名>`
- 查看 Which-key：按 `<leader>` 等待弹出菜单

## 🔗 相关资源

- [AstroNvim 官方文档](https://docs.astronvim.com/)
- [AstroCommunity 仓库](https://github.com/AstroNvim/astrocommunity)
- [本配置仓库](../)
