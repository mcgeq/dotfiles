;;; custom-example.el --- 用户自定义配置示例 -*- lexical-binding: t; -*-

;; Filename: custom-example.el
;; Description: 用户自定义配置模板
;; Author: mcge <mcgeq@outlook.com>
;; Created: 2025-12-06

;;; Commentary:
;;
;; 这是一个配置示例文件，展示如何自定义 Emacs 配置
;; 
;; 使用方法:
;;   1. 复制此文件为 custom.el
;;   2. 修改下面的配置项
;;   3. 重启 Emacs
;;
;; 注意: custom.el 已被添加到 .gitignore，不会被提交
;;

;;; Code:

;; =============================================================================
;; 路径配置
;; =============================================================================

;; 配置基础目录
;; Windows: "D:/config"
;; Linux/macOS: (expand-file-name "~")
(setq mcg-base-dir "D:/config")

;; Org 文件目录
;; (setq mcg-org-dir (concat mcg-base-dir "/org"))

;; 博客目录
;; (setq mcg-blog-dir (concat mcg-base-dir "/workspaces/blog/mcge-blog"))


;; =============================================================================
;; 模块配置
;; =============================================================================

;; 使用 mcg! 宏声明启用的模块
;; 示例:
;;   :core           - 启用核心模块（必需）
;;   :ui doom-modeline  - 启用 UI 类别的 doom-modeline 模块
;;   :editor line-number indent  - 启用多个编辑器模块
;;   :lang (rust lua typescript)  - 启用多个语言支持
;;   :org -org-download  - 禁用特定模块（前缀 -）

(mcg! :core                             ; 核心模块（必需）
      
      :ui doom-modeline                 ; UI 模块
      
      :editor                           ; 编辑器增强
      line-number
      indent
      editor
      treesit
      
      :completion                       ; 补全系统
      marginalia
      vertico
      embark
      yasnippet
      
      :keybindings                      ; 键盘绑定
      keymaps
      vundo
      
      :tools                            ; 工具
      sort-tab
      auto-save
      recentf
      generic
      symbol-overlay
      
      :search                           ; 搜索
      blink-search
      color-rg
      helpful
      
      :input                            ; 输入法
      rime
      wraplish
      fingertip
      
      :git                              ; 版本控制
      magit
      
      :lang                             ; 语言支持
      lsp-bridge
      (rust lua cpp typescript web)
      
      :org                              ; Org Mode
      org
      org-agenda
      org-download
      capture-hugo
      consult-todo
      org-numbering
      
      :docs                             ; 文档
      markdown
      grip-mode)


;; =============================================================================
;; UI 配置
;; =============================================================================

;; 主题选择
;; 可选值: 'doom-one 'modus-operandi 'modus-vivendi 'ef-day 'ef-night
;; (setq mcg-theme 'doom-one)

;; 字体配置
;; 默认字体
(setq mcg-font-family "FiraCode Nerd Font Mono")
(setq mcg-font-size 14)

;; 中文字体
;; (setq mcg-cjk-font "Microsoft Yahei")

;; 图标字体
;; (setq mcg-icon-font "FiraCode Nerd Font")


;; =============================================================================
;; LSP 配置
;; =============================================================================

;; LSP 后端选择
;; 可选值: 'lsp-bridge 'eglot 'lsp-mode
(setq mcg-lsp-backend 'lsp-bridge)

;; LSP 服务器配置目录
;; (setq mcg-lsp-server-dir (concat mcgemacs-root-dir "/langservers"))


;; =============================================================================
;; 补全系统配置
;; =============================================================================

;; 补全系统选择
;; 可选值: 'vertico 'ivy 'helm
(setq mcg-completion-system 'vertico)

;; Vertico 显示行数
;; (setq vertico-count 15)

;; 是否启用拼音搜索
;; (setq mcg-enable-pinyin-search t)


;; =============================================================================
;; 功能开关
;; =============================================================================

;; Dashboard
(setq mcg-enable-dashboard t)

;; Line numbers
(setq mcg-enable-line-numbers t)

;; Auto-save
(setq mcg-enable-auto-save t)

;; Symbol overlay
(setq mcg-enable-symbol-overlay t)

;; Treesit (tree-sitter)
(setq mcg-enable-treesit t)


;; =============================================================================
;; Org Mode 配置
;; =============================================================================

;; 是否启用 Org mode
(setq mcg-enable-org t)

;; Org 文件位置
;; (setq org-directory (concat mcg-base-dir "/org"))

;; Agenda 文件
;; (setq org-agenda-files '("~/org/tasks.org" "~/org/projects.org"))

;; Capture templates (示例)
;; (setq org-capture-templates
;;       '(("t" "Task" entry (file+headline "~/org/tasks.org" "Tasks")
;;          "* TODO %?\n  %i\n  %a")
;;         ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
;;          "* %?\n  %i\n  %a")))


;; =============================================================================
;; 输入法配置
;; =============================================================================

;; Rime 用户数据目录
;; Windows: (concat mcg-base-dir "/rime")
;; Linux: "~/.config/fcitx/rime"
;; (setq mcg-rime-user-data-dir (concat mcg-base-dir "/rime"))


;; =============================================================================
;; 性能配置
;; =============================================================================

;; GC 阈值（字节）
;; 默认: 800000 (800KB)
;; 推荐: 100000000 (100MB) for better performance
;; (setq gc-cons-threshold 100000000)

;; 启动后恢复 GC 阈值
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold 800000)))

;; 是否启用性能监控
;; (setq mcg-benchmark-enabled nil)


;; =============================================================================
;; 编辑器行为
;; =============================================================================

;; 是否使用空格缩进
;; (setq-default indent-tabs-mode nil)

;; Tab 宽度
;; (setq-default tab-width 4)

;; 行号显示模式
;; 可选值: 'relative 'absolute nil
;; (setq display-line-numbers-type 'relative)

;; 自动保存间隔（秒）
;; (setq auto-save-interval 5)


;; =============================================================================
;; 键盘绑定 (可选)
;; =============================================================================

;; 自定义键盘绑定示例
;; (global-set-key (kbd "C-c C-c") #'my-custom-function)


;; =============================================================================
;; 语言特定配置
;; =============================================================================

;; Python
;; (setq python-shell-interpreter "python3")

;; Rust
;; (setq rust-format-on-save t)

;; JavaScript/TypeScript
;; (setq js-indent-level 2)
;; (setq typescript-indent-level 2)


;; =============================================================================
;; 其他配置
;; =============================================================================

;; 启动时显示的 buffer
;; 可选值: "*scratch*" "*dashboard*" 或文件路径
;; (setq initial-buffer-choice "*scratch*")

;; 是否显示启动时间
;; (setq mcg-display-startup-time t)


;;; custom-example.el ends here
