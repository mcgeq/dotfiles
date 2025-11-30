-- AstroCommunity 插件统一配置
-- 所有社区插件集中管理，便于维护
-- 文档: https://github.com/AstroNvim/astrocommunity

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  
  -- ============================================
  -- 语言包 - 按技术栈分类
  -- ============================================
  
  -- === 前端开发 ===
  { import = "astrocommunity.pack.typescript-all-in-one" }, -- TypeScript + JavaScript + React
  { import = "astrocommunity.pack.vue" }, -- Vue.js 框架
  { import = "astrocommunity.pack.biome" }, -- Biome (Linting + 格式化 + Import 排序，支持 Vue)
  { import = "astrocommunity.pack.html-css" }, -- HTML + CSS + SCSS
  { import = "astrocommunity.pack.tailwindcss" }, -- Tailwind CSS
  
  -- === 后端开发 ===
  { import = "astrocommunity.pack.rust" }, -- Rust
  { import = "astrocommunity.pack.go" }, -- Go
  { import = "astrocommunity.pack.python-ruff" }, -- Python (使用 Ruff)
  { import = "astrocommunity.pack.cpp" }, -- C/C++
  { import = "astrocommunity.pack.zig" }, -- Zig
  { import = "astrocommunity.pack.cmake" }, -- CMake 构建系统
  
  -- === 配置和数据格式 ===
  { import = "astrocommunity.pack.json" }, -- JSON
  { import = "astrocommunity.pack.yaml" }, -- YAML
  { import = "astrocommunity.pack.toml" }, -- TOML
  
  -- === 脚本和工具 ===
  { import = "astrocommunity.pack.lua" }, -- Lua (Neovim 配置)
  { import = "astrocommunity.pack.bash" }, -- Bash 脚本
  
  -- === DevOps ===
  { import = "astrocommunity.pack.docker" }, -- Docker
  
  -- === 文档 ===
  { import = "astrocommunity.pack.markdown" }, -- Markdown
  
  -- === 版本控制 ===
  { import = "astrocommunity.pack.jj" }, -- Jujutsu VCS
  
  -- === 可选语言包（暂不启用）===
  -- { import = "astrocommunity.pack.svelte" }, -- Svelte 框架
  -- { import = "astrocommunity.pack.xml" }, -- XML
  -- { import = "astrocommunity.pack.sql" }, -- SQL
  -- { import = "astrocommunity.pack.dart" }, -- Dart + Flutter
  -- { import = "astrocommunity.pack.kotlin" }, -- Kotlin
  -- { import = "astrocommunity.pack.java" }, -- Java
  -- { import = "astrocommunity.pack.clojure" }, -- Clojure
  
  -- ============================================
  -- Fuzzy Finder（统一使用 snacks-picker）
  -- ============================================
  { import = "astrocommunity.fuzzy-finder.snacks-picker" },
  -- 已移除冗余的：fzf-lua, namu-nvim
  
  -- ============================================
  -- Git 工具
  -- ============================================
  { import = "astrocommunity.git.blame-nvim" }, -- Git blame 信息
  { import = "astrocommunity.git.gist-nvim" }, -- Gist 管理
  { import = "astrocommunity.git.diffview-nvim" }, -- 更好的 diff 视图
  { import = "astrocommunity.git.gitlinker-nvim" }, -- 生成 GitHub/GitLab 链接
  
  -- ============================================
  -- 补全增强
  -- ============================================
  { import = "astrocommunity.completion.blink-cmp" },
  { import = "astrocommunity.completion.blink-cmp-emoji" },
  { import = "astrocommunity.completion.blink-cmp-git" },
  
  -- ============================================
  -- 编辑增强
  -- ============================================
  { import = "astrocommunity.editing-support.bigfile-nvim" }, -- 大文件优化
  { import = "astrocommunity.editing-support.conform-nvim" }, -- 格式化工具
  { import = "astrocommunity.editing-support.nvim-treesitter-context" }, -- 显示上下文
  { import = "astrocommunity.editing-support.todo-comments-nvim" }, -- TODO 高亮
  { import = "astrocommunity.editing-support.zen-mode-nvim" }, -- 专注模式
  { import = "astrocommunity.editing-support.yanky-nvim" }, -- 增强剪贴板
  { import = "astrocommunity.editing-support.neogen" }, -- 代码文档生成
  { import = "astrocommunity.editing-support.refactoring-nvim" }, -- 代码重构工具
  
  -- ============================================
  -- 搜索和替换
  -- ============================================
  { import = "astrocommunity.search.nvim-spectre" }, -- 全局搜索和替换
  
  -- ============================================
  -- 项目管理
  -- ============================================
  { import = "astrocommunity.project.project-nvim" }, -- 自动检测项目根目录
  
  -- ============================================
  -- 诊断工具
  -- ============================================
  { import = "astrocommunity.diagnostics.trouble-nvim" },
  
  -- ============================================
  -- 调试增强
  -- ============================================
  { import = "astrocommunity.debugging.nvim-dap-virtual-text" }, -- DAP 虚拟文本
  { import = "astrocommunity.debugging.persistent-breakpoints-nvim" }, -- 持久化断点
  
  -- ============================================
  -- 文件浏览
  -- ============================================
  { import = "astrocommunity.file-explorer.mini-files" },
  -- 可选：Oil.nvim - 像编辑文件一样编辑目录
  -- { import = "astrocommunity.file-explorer.oil-nvim" },
  
  -- ============================================
  -- LSP 增强
  -- ============================================
  { import = "astrocommunity.lsp.actions-preview-nvim" },
  
  -- ============================================
  -- 界面美化
  -- ============================================
  { import = "astrocommunity.indent.indent-blankline-nvim" }, -- 缩进线
  { import = "astrocommunity.utility.noice-nvim" }, -- 美化 UI 和通知
  { import = "astrocommunity.color.ccc-nvim" }, -- 颜色预览
  
  -- ============================================
  -- 注释增强
  -- ============================================
  { import = "astrocommunity.comment.ts-comments-nvim" },
  
  -- ============================================
  -- 动作/移动增强
  -- ============================================
  { import = "astrocommunity.motion.flash-nvim" }, -- 快速跳转
  { import = "astrocommunity.motion.mini-surround" }, -- 环绕操作
  { import = "astrocommunity.motion.mini-ai" }, -- 增强的文本对象（ia/aa等）
  { import = "astrocommunity.motion.nvim-spider" }, -- 智能 w/e/b 移动，跳过标点
  { import = "astrocommunity.motion.harpoon" }, -- 文件快速标记和跳转
  
  -- ============================================
  -- Markdown 增强
  -- ============================================
  { import = "astrocommunity.markdown-and-latex.markdown-preview-nvim" },
  
  -- ============================================
  -- 可选插件（按需启用）
  -- ============================================
  
  -- === 代码运行 ===
  -- { import = "astrocommunity.code-runner.overseer-nvim" }, -- 任务运行器
  -- { import = "astrocommunity.code-runner.sniprun" }, -- 代码片段运行
  
  -- === 测试工具 ===
  { import = "astrocommunity.test.neotest" }, -- 测试框架
  -- { import = "astrocommunity.test.nvim-coverage" }, -- 测试覆盖率
  
  -- === 工作流优化 ===
  { import = "astrocommunity.workflow.hardtime-nvim" }, -- 培养好习惯（可能影响流畅度）
  -- { import = "astrocommunity.workflow.precognition-nvim" }, -- 显示可用操作（学习用）
  
  -- === 实用工具 ===
  { import = "astrocommunity.utility.neodim" }, -- 未使用代码变暗（影响性能）
  { import = "astrocommunity.utility.nvim-toggler" }, -- 文本切换（true/false等）
  
  -- ============================================
  -- 已移除的插件及原因
  -- ============================================
  
  -- auto-save-nvim - 可能意外保存，推荐 Ctrl+S 手动保存
  -- comment-box-nvim - 功能重，snippets 足够
  -- nvim-regexplainer - 不常用
  -- nvim-origami - 可能与 treesitter 冲突
  -- lsp_lines-nvim - 可能与 virtual_text 冲突
  -- gitgraph-nvim - 功能较重
  -- executor-nvim - 与 overseer 功能重叠
  -- error-lens-nvim - 与 aerial.nvim 冲突
  -- fzf-lua, namu-nvim - 冗余的 fuzzy finder
}
