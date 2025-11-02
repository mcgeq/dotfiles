-- 推荐的 AstroNvim 社区插件补充
-- 根据当前配置特点推荐的插件

---@type LazySpec
return {
  -- ============================================
  -- 编辑支持类 (Editing Support)
  -- ============================================

  -- 自动保存 - 配合你的自动更新时间戳功能
  { import = "astrocommunity.editing-support.auto-save-nvim" },

  -- 代码文档生成 - 配合你的文件头 snippets
  { import = "astrocommunity.editing-support.neogen" },

  -- 更好的注释框样式 - 可以美化文件头
  { import = "astrocommunity.editing-support.comment-box-nvim" },

  -- 代码重构工具
  { import = "astrocommunity.editing-support.refactoring-nvim" },

  -- 正则表达式解释器 - 方便理解正则
  { import = "astrocommunity.editing-support.nvim-regexplainer" },

  -- 代码折叠增强
  { import = "astrocommunity.editing-support.nvim-origami" },

  -- ============================================
  -- 诊断类 (Diagnostics)
  -- ============================================

  -- 错误高亮增强 - 更好的错误显示
  { import = "astrocommunity.diagnostics.error-lens-nvim" },

  -- LSP 诊断行内显示
  { import = "astrocommunity.diagnostics.lsp_lines-nvim" },

  -- ============================================
  -- Git 增强类
  -- ============================================

  -- 更好的 diff 视图
  { import = "astrocommunity.git.diffview-nvim" },

  -- Git 链接生成器
  { import = "astrocommunity.git.gitlinker-nvim" },

  -- Git 图形化
  { import = "astrocommunity.git.gitgraph-nvim" },

  -- ============================================
  -- 代码运行类 (Code Runner)
  -- ============================================

  -- 代码执行器
  { import = "astrocommunity.code-runner.executor-nvim" },

  -- 代码片段运行
  { import = "astrocommunity.code-runner.sniprun" },

  -- 任务运行器
  { import = "astrocommunity.code-runner.overseer-nvim" },

  -- ============================================
  -- 工作流类 (Workflow)
  -- ============================================

  -- 帮助建立良好的编辑习惯
  { import = "astrocommunity.workflow.hardtime-nvim" },

  -- 发现导航动作
  { import = "astrocommunity.workflow.precognition-nvim" },

  -- ============================================
  -- 实用工具类 (Utility)
  -- ============================================

  -- 未使用的代码变暗
  { import = "astrocommunity.utility.neodim" },

  -- 文本反转切换（true/false, if/else 等）
  { import = "astrocommunity.utility.nvim-toggler" },

  -- ============================================
  -- 测试类 (Test)
  -- ============================================

  -- 测试框架
  { import = "astrocommunity.test.neotest" },

  -- 测试覆盖率显示
  { import = "astrocommunity.test.nvim-coverage" },

  -- ============================================
  -- 调试类 (Debugging)
  -- ============================================

  -- DAP 增强
  { import = "astrocommunity.debugging.nvim-dap-virtual-text" },
  { import = "astrocommunity.debugging.persistent-breakpoints-nvim" },

  -- ============================================
  -- Markdown 增强
  -- ============================================

  -- Markdown 增强（如果你已经在用 markdown）
  -- { import = "astrocommunity.markdown-and-latex.vim-markdown" },

  -- ============================================
  -- 文件浏览器增强
  -- ============================================

  -- 更好的文件浏览器
  { import = "astrocommunity.file-explorer.oil-nvim" },
}

