;;; init-fingertip.el -- Config for Fingertip -*- lexical-binding: t; -*-

;; Filename: init-fingertip.el
;; Description: Config for Fingertip
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:35>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Fingertip
;;

;;; Installation:
;;
;; Put init-fingertip.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-fingertip)
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
(require 'fingertip)

;;; Code:

(dolist (hook (list
               'bash-ts-mode-hook
               'c++-mode-hook
               'c++-ts-mode-hook
               'c-mode-common-hook
               'c-mode-hook
               'c-ts-mode-hook
               'cmake-ts-mode-hook
               'coffee-mode-hook
               'conf-toml-mode-hook
               'css-mode-hook
               'css-ts-mode-hook
               'emacs-lisp-mode-hook
               'go-mode-hook
               'haskell-mode-hook
               'ielm-mode-hook
               'jade-mode-hook
               'java-mode-hook
               'js-mode-hook
               'js-ts-mode-hook
               'json-ts-mode-hook
               'kotlin-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'llvm-mode-hook
               'lua-mode-hook
               'makefile-gmake-mode-hook
               'markdown-ts-mode-hook
               'maxima-mode-hook
               'mojo-mode-hook
               'nim-mode-hook
               'php-mode-hook
               'php-ts-mode-hook
               'python-mode-hook
               'python-ts-mode-hook
               'qmake-mode-hook
               'qml-mode-hook
               'ruby-mode-hook
               'rust-mode-hook
               'rust-ts-mode-hook
               'sh-mode-hook
               'swift-mode-hook
               'toml-ts-mode-hook
               'typescript-mode-hook
               'typescript-ts-mode-hook
               'web-mode-hook
               'zig-mode-hook
               'fsharp-mode-hook
               'clojure-mode-hook
               'clojure-ts-mode-hook
               'cider-repl-mode-hook
               ))
  (add-hook hook #'(lambda ()
                     (when (or
                            (not (buffer-file-name))
                            (not (string-equal (file-name-extension (buffer-file-name)) "chat")))
                       (fingertip-mode 1)))))

;;; `https://github.com/manateelazycat/lazycat-emacs/blob/master/site-lisp/config/init-key.el'
;;; ### Fingertip ###
;;; --- 结构化编程
(lazy-load-unset-keys
 '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b" "M-)")
 fingertip-mode-map)                    ;卸载按键
(defvar fingertip-key-alist nil)
(setq fingertip-key-alist
      '(
        ;; 移动
        ("M-n" . fingertip-jump-left)
        ("M-p" . fingertip-jump-right)
        ;; 符号插入
        ("%" . fingertip-match-paren)            ;括号跳转
        ("(" . fingertip-open-round)             ;智能 (
        ("[" . fingertip-open-bracket)           ;智能 [
        ("{" . fingertip-open-curly)             ;智能 {
        (")" . fingertip-close-round)            ;智能 )
        ("]" . fingertip-close-bracket)          ;智能 ]
        ("}" . fingertip-close-curly)            ;智能 }
        ("（" . fingertip-open-chinese-round)    ;智能 （
        ("「" . fingertip-open-chinese-bracket)  ;智能 「
        ("【" . fingertip-open-chinese-curly)    ;智能 【
        ("）" . fingertip-close-chinese-round)   ;智能 ）
        ("」" . fingertip-close-chinese-bracket) ;智能 」
        ("】" . fingertip-close-chinese-curly)   ;智能 】
        ("\"" . fingertip-double-quote)          ;智能 "
        ("'" . fingertip-single-quote)           ;智能 '
        ("=" . fingertip-equal)                  ;智能 =
        ("SPC" . fingertip-space)                ;智能 space
        ("RET" . fingertip-newline)              ;智能 newline
        ;; 删除
        ("M-o" . fingertip-backward-delete) ;向后删除
        ("C-d" . fingertip-forward-delete)  ;向前删除
        ("C-k" . fingertip-kill)            ;向前kill
        ;; 包围
        ("M-\"" . fingertip-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
        ("M-'" . fingertip-wrap-single-quote) ;用 ' ' 包围对象, 或跳出字符串
        ;;("M-[" . fingertip-wrap-bracket)      ;用 [ ] 包围对象
        ("M-{" . fingertip-wrap-curly)        ;用 { } 包围对象
        ("M-(" . fingertip-wrap-round)        ;用 ( ) 包围对象
        ("M-)" . fingertip-unwrap)            ;去掉包围对象
        ;; 跳出并换行缩进
        ("M-:" . fingertip-jump-out-pair-and-newline) ;跳出括号并换行
        ;; 向父节点跳动
        ("C-j" . fingertip-jump-up)
        ))
(lazy-load-set-keys fingertip-key-alist fingertip-mode-map)

(provide 'init-fingertip)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fingertip.el ends here
