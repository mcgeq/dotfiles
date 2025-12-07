;;; example-custom.el --- Org Mode 自定义配置示例 -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 mcge

;; Author: mcge <mcgeq@outlook.com>
;; Keywords: org, configuration, example
;; Version: 2.1

;;; Commentary:

;; 这是 Org Mode 新配置系统的自定义示例文件。
;; 
;; 使用方法：
;; 1. 复制此文件到你的配置目录（如 ~/.emacs.d/）
;; 2. 重命名为 my-org-config.el
;; 3. 根据需要修改配置
;; 4. 在 init.el 中加载：
;;    (load "~/.emacs.d/my-org-config.el" t)
;;
;; 注意：这些配置需要在加载 init-org-loader 之前设置

;;; Code:

;;; ====================================================================
;;; 示例 1: 基础配置
;;; ====================================================================

;; 自定义 Org 目录位置
;; (setq mcg-org-directory "~/Documents/MyOrg")
;; (setq mcg-org-agenda-directory (concat mcg-org-directory "/Agenda"))
;; (setq mcg-org-notes-directory (concat mcg-org-directory "/Notes"))
;; (setq mcg-org-blog-directory (concat mcg-org-directory "/Blog"))

;; 自定义博客根目录
;; (setq mcg-blog-base-directory "~/Projects/my-blog")

;;; ====================================================================
;;; 示例 2: 功能开关
;;; ====================================================================

;; 启用/禁用功能
;; (setq mcg-org-enable-pretty-symbols t)      ; 符号美化
;; (setq mcg-org-enable-mixed-pitch t)         ; 混合字体
;; (setq mcg-org-enable-visual-fill t)         ; 视觉填充列
;; (setq mcg-org-enable-chinese-calendar t)    ; 中文日历
;; (setq mcg-org-enable-hugo-integration nil)  ; 禁用 Hugo 集成

;;; ====================================================================
;;; 示例 3: 地理位置配置（用于日出日落计算）
;;; ====================================================================

;; 北京
;; (setq mcg-calendar-location-longitude 116.40
;;       mcg-calendar-location-latitude 39.90)

;; 上海
;; (setq mcg-calendar-location-longitude 121.47
;;       mcg-calendar-location-latitude 31.23)

;; 广州
;; (setq mcg-calendar-location-longitude 113.27
;;       mcg-calendar-location-latitude 23.13)

;; 深圳
;; (setq mcg-calendar-location-longitude 114.06
;;       mcg-calendar-location-latitude 22.55)

;;; ====================================================================
;;; 示例 4: Windows 用户配置
;;; ====================================================================

;; Windows 用户可能需要使用绝对路径
;; (when (eq system-type 'windows-nt)
;;   (setq mcg-org-directory "D:/Documents/Org")
;;   (setq mcg-blog-base-directory "D:/Projects/blog"))

;;; ====================================================================
;;; 示例 5: 条件配置（根据环境）
;;; ====================================================================

;; 在图形界面启用某些功能
;; (when (display-graphic-p)
;;   (setq mcg-org-enable-mixed-pitch t)
;;   (setq mcg-org-enable-visual-fill t))

;; 在终端中禁用
;; (unless (display-graphic-p)
;;   (setq mcg-org-enable-mixed-pitch nil)
;;   (setq mcg-org-enable-pretty-symbols nil))

;;; ====================================================================
;;; 示例 6: 完整配置（推荐）
;;; ====================================================================

;; 取消注释以下代码块使用完整配置

;; ===== 目录配置 =====
;; (setq mcg-org-directory
;;       (if (eq system-type 'windows-nt)
;;           "D:/Documents/Org"
;;         "~/Documents/Org"))
;; 
;; (setq mcg-org-agenda-directory (concat mcg-org-directory "/Agenda"))
;; (setq mcg-org-notes-directory (concat mcg-org-directory "/Notes"))
;; (setq mcg-org-blog-directory (concat mcg-org-directory "/Blog"))
;; (setq mcg-org-archive-directory (concat mcg-org-directory "/Archive"))
;; 
;; ===== 博客配置 =====
;; (setq mcg-blog-base-directory
;;       (if (eq system-type 'windows-nt)
;;           "D:/Projects/my-blog"
;;         "~/Projects/my-blog"))
;; 
;; ===== 功能开关 =====
;; (setq mcg-org-enable-pretty-symbols t)
;; (setq mcg-org-enable-mixed-pitch (display-graphic-p))
;; (setq mcg-org-enable-visual-fill (display-graphic-p))
;; (setq mcg-org-enable-chinese-calendar t)
;; (setq mcg-org-enable-hugo-integration t)
;; 
;; ===== 地理位置 =====
;; (setq mcg-calendar-location-longitude 116.40)  ; 北京
;; (setq mcg-calendar-location-latitude 39.90)

;;; ====================================================================
;;; 示例 7: Org Mode Hook 自定义
;;; ====================================================================

;; 自定义 org-mode hook
;; (defun my-org-mode-setup ()
;;   "我的 Org mode 自定义设置。"
;;   ;; 增加行间距
;;   (setq-local line-spacing 3)
;;   ;; 启用自动换行
;;   (auto-fill-mode 1)
;;   ;; 启用拼写检查（如果安装了）
;;   (when (executable-find "aspell")
;;     (flyspell-mode 1)))
;; 
;; (add-hook 'org-mode-hook #'my-org-mode-setup)

;;; ====================================================================
;;; 示例 8: 自定义 TODO 关键词
;;; ====================================================================

;; 如果需要自定义 TODO 关键词，在加载 org 后设置
;; (with-eval-after-load 'org
;;   (setq org-todo-keywords
;;         '((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))

;;; ====================================================================
;;; 示例 9: 自定义 Capture 模板
;;; ====================================================================

;; 添加自定义 capture 模板
;; (with-eval-after-load 'org-capture
;;   (add-to-list 'org-capture-templates
;;                '("m" "Meeting" entry
;;                  (file+headline mcg-org-task-file "Meetings")
;;                  "* MEETING %? :meeting:\n%U\n** Attendees\n** Notes\n"
;;                  :empty-lines 1)))

;;; ====================================================================
;;; 示例 10: 模块化加载控制
;;; ====================================================================

;; 如果不想自动加载所有模块，可以手动控制
;; 
;; 方法 1: 只加载必需模块
;; (with-eval-after-load 'init-org-loader
;;   (mcg-org-load-required))  ; 只加载必需模块
;; 
;; 方法 2: 选择性加载
;; (with-eval-after-load 'init-org-loader
;;   (mcg-org-load-module 'vars)
;;   (mcg-org-load-module 'core)
;;   (mcg-org-load-module 'agenda)
;;   ;; 不加载 capture, download 等模块
;;   )

;;; ====================================================================
;;; 验证配置
;;; ====================================================================

;; 配置加载后，可以运行以下命令验证：
;; M-x mcg-org-show-status        ; 查看配置状态
;; M-x mcg-org-list-modules       ; 列出所有模块
;; M-x mcg-org-validate-config    ; 验证配置

;;; ====================================================================
;;; 提示和技巧
;;; ====================================================================

;; 1. 使用 customize 接口设置变量
;;    M-x customize-group RET mcg-org RET
;;
;; 2. 查看变量当前值
;;    C-h v mcg-org-directory RET
;;
;; 3. 重新加载模块（如果修改了配置）
;;    M-x mcg-org-reload-module RET core RET
;;
;; 4. 查看加载的文件列表
;;    M-x list-load-path-shadows
;;
;; 5. 调试配置问题
;;    (setq debug-on-error t)
;;    重启 Emacs 查看详细错误

;;; ====================================================================
;;; 更多信息
;;; ====================================================================

;; 完整文档：org/README-ORG-CONFIG.org
;; 快速开始：org/QUICK-START.org
;; 迁移指南：org/MIGRATION-GUIDE.org
;; 验证报告：org/VERIFICATION-REPORT.org

(provide 'example-custom)
;;; example-custom.el ends here
