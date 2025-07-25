;;; init-generic.el -- Config for Emacs Base Options -*- lexical-binding: t; -*-

;; Filename: init-generic.el
;; Description: Config for Emacs Base Options
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:12>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Emacs Base Options
;;

;;; Installation:
;;
;; Put init-generic.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-generic)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-aider RET
;;

;;; Change log:
;;

;;; Require:

;;; Code:

(if *is-win32p* (setq system-time-locale "C"))
;; Coding System UTF-8
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (set-selection-coding-system 'utf-8))
(prefer-coding-system                   'utf-8)
(set-default-coding-systems             'utf-8)
(set-terminal-coding-system             'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-buffer-file-coding-system          'utf-8)
;(setq default-buffer-file-coding-system 'utf-8)

;; 增加长行处理性能
(setq bidi-inhibit-bpa                            t)
;;增加长行处理性能
(setq-default bidi-paragraph-diretion             'left-to-right)

;; 增加 IO 性能
(setq process-adaptive-read-buffering            nil)
(setq read-process-output-max                    (* 1024 1024))
;;行号
;(global-display-line-numbers-mode)
;;(fset 'yes-or-no-p'                             'y-or-n-p)
;;以 y/n 代表 yes/no
;;(defalias 'yes-or-no-p                          'y-or-n-p)
(setq use-short-answers                          t)
;;指针不闪动
(blink-cursor-mode                              -1)
;;标记高亮
(transient-mark-mode                             1)
;;Word 移动支持 FooBar 的格式
(global-subword-mode                             1)
;;不弹出对话框
(setq use-dialog-box                             nil)
;;禁止启动屏幕
(setq inhibit-startup-screen                     t)
(setq initial-scratch-message                   (concat ";; Happy hacking, "
                                                  (capitalize user-login-name)
                                                    " - Emacs ❤  you!\n\n"))

;;设定自动缩进的注释风格
(setq-default comment-style                     'indent)
;;关闭出错时的提示声
(setq ring-bell-function                        'ignore)
;;设置默认的主模式为 TEXT 模式
(setq-default major-mode                        'text-mode)
;;粘贴于光标处，而不是鼠标指针处
(setq mouse-yank-at-point                       t)
;;支持 emacs 和外部程序的粘贴
(setq x-select-enable-clipboard                 t)
(setq select-enable-clipboard                   t)
;;分屏的时候使用上下分屏
(setq split-width-threshold                     nil)
;;使用字体缓存，避免卡顿
(setq inhibit-compacting-font-caches            t)
;;退出时自动杀掉进程
(setq confirm-kill-processes                    nil)
;;避免 magit 报错
;;(setq async-bytecomp-allowed-packages           nil)
;;按照中文折行
(setq word-wrap-by-category                     t)
(setq make-backup-files                         nil)
(setq auto-save-default                         nil)
;;禁止文件备份
(setq backup-inhibited                          t)
;;显示列号
(setq column-number-mode                        t)
;;避免默认自动选择
(setq completion-auto-select                    nil)
;;设置缩放的模式
(setq ad-redefinition-action                    'accept)
;;关闭 redefine warning
(setq frame-resize-pixelwise                    t)
;;平滑进行半屏滚动，避免滚动后 recenter 操作
(setq scroll-step                               1
      scroll-conservatively                     10000)
;;设置光标
(setq-default cursor-type '(bar . 5))

;; 启用`visual-line-mode'
(global-visual-line-mode t)

;; set `fill-column'
(setq-default fill-column 100)

;; `auto-fill-mode'
(setq-default auto-fill-function 'do-auto-fill)
;;(global-auto-fill-mode t)
;; 括号匹配
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

(provide 'init-generic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-generic.el ends here
