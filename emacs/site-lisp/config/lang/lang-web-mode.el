;;; lang-web-mode -- Config for Modern CPP Font -*- lexical-binding: t; -*-

;; Filename: lang-cpp
;; Description: Config for Modern CPP Font
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: 2025-01-04 15:46:46
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Modern CPP Font
;;

;;; Installation:
;;
;; Put lang-web-mode to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lang-cpp)
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
(require 'web-mode)
(require 'js)
(require 'instant-rename-tag)
(require 'highlight-matching-tag)
(require 'css-mode)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; OS Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep 'cocoa)
  ;; Initialize environment from user's shell to make eshell know every PATH by other shell.
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(setq web-mode-enable-auto-quoting nil) ;disable automatic insertion of double quotes, not easy to use if cursor in string

(highlight-matching-tag 1)

;; css
(setq css-indent-offset 2)

;; Emmit.
(setq web-mode-tag-auto-close-style 2) ;2 mean auto-close with > and </.
(setq web-mode-content-types-alist '(("vue" . "\\.vue\\'")))
(setq web-mode-css-indent-offset 2)       ; CSS 缩进（包含 HTML 的 CSS 部分及 CSS/LESS/SASS 文件）
(setq web-mode-code-indent-offset 2)      ; JavaScript 缩进（包含 SCRIPT 部分及 JS/JSX/TS/TSX 文件）
(setq web-mode-markup-indent-offset 2)    ; HTML 缩进（包含 HTML 文件及 Vue 的 TEMPLATE 部分）
(setq web-mode-enable-css-colorization t) ; 开启 CSS 颜色值显示
(setq web-mode-enable-auto-indentation nil) ; 禁用粘贴时自动格式化
(setq web-mode-enable-current-column-highlight nil)

(with-eval-after-load 'web-mode
  (setq tab-width 2))

;; We-mode.
;; (lazy-load-set-keys fingertip-key-alist web-mode-map)
(lazy-load-local-keys
 '(
   ("M-s-SPC" . web-mode-element-content-select)
   ("C-s-l" . web-mode-element-clone)
   ("C-M-SPC" . web-mode-mark-and-expand)
   ("C-:" . web-mode-comment-or-uncomment)
   ("C-M-SPC" . mark-sexp)
   ("M-R" . instant-rename-tag)
   )
 web-mode-map
 "web-mode-extension")

(provide 'lang-web-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-web-mode.el ends here