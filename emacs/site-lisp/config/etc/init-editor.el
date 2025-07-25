;;; init-editor.el -- Config for Editor -*- lexical-binding: t; -*-

;; Filename: init-editor.el
;; Description: Config for Editor
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:18>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Editor
;;

;;; Installation:
;;
;; Put init-editor.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-editor)
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
(require 'delsel)
(require 'beacon)
(require 'htmlize)
(require 'display-line-numbers)

;;; Code:
(add-hook 'after-init-hook 'delete-selection-mode)

(defvar mcg/indent-tabs-modes-list
  '(makefile-gmake-mode
    makefile-mode
    )
  )

(defun mcg/toggle-indent-tabs-mode ()
  "Toogle indent tabs mode."
  (if (member major-mode mcg/indent-tabs-modes-list)
      (setq indent-tabs-mode t)
    (setq indent-tabs-mode nil)))

(add-hook 'after-change-major-mode-hook 'mcg/toggle-indent-tabs-mode)

;; 光标彩虹插件

(add-hook 'after-init-hook #'beacon-mode)

(setq beacon-lighter "")
;; 配置 beacon 相关的选项
(setq beacon-blink-when-point-moves-vertically nil) ; default nil
(setq beacon-blink-when-point-moves-horizontally nil) ; default nil
(setq beacon-blink-when-buffer-changes t) ; default t
(setq beacon-blink-when-window-scrolls t) ; default t
(setq beacon-blink-when-window-changes t) ; default t
(setq beacon-blink-when-focused nil) ; default nil

(setq beacon-blink-duration 0.3) ; default 0.3
(setq beacon-blink-delay 0.3) ; default 0.3
(setq beacon-size 20) ; default 40
;; (setq beacon-color "yellow") ; default 0.5
(setq beacon-color 0.5) ; default 0.5

;; 将 term-mode 添加到不需要闪烁的模式列表中
(add-to-list 'beacon-dont-blink-major-modes 'term-mode)
(beacon-mode 1)

;; show line number

(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook     #'display-line-numbers-mode)
(add-hook 'yaml-mode-hook     #'display-line-numbers-mode)
(add-hook 'conf-mode-hook     #'display-line-numbers-mode)
(add-hook 'markdown-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook     'hs-minor-mode)   ;代码折叠
;; line number align right
(setq display-line-numbers-width-start t)

(provide 'init-editor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-editor.el ends here
