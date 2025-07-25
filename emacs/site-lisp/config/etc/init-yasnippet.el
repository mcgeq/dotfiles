;;; init-yasnippet.el -- Config for yasnippet -*- lexical-binding: t; -*-

;; Filename: init.el
;; Description: Config for yasnippet
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:36>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for yasnippet
;;

;;; Installation:
;;
;; Put init-yasnippet.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-yasnippet)
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
(require 'js2-mode)
(require 'yasnippet)

;;; Code:

;; 全局启用 yasnippet
(yas-global-mode 1)


(add-hook 'prog-mode-hook
          #'(lambda ()
            ;; 定义从 Git 配置中获取用户名和电子邮件的函数
            (defun get-git-user-name ()
              "Get the Git configured user name."
              (interactive)
              (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.name")))

            (defun get-git-user-email ()
              "Get the Git configured user email."
              (interactive)
              (replace-regexp-in-string "\n$" "" (shell-command-to-string "git config --get user.email")))

            ;; 添加多个自定义的片段目录
            (let ((custom-snippet-dirs
                   (list (concat mcgemacs-root-dir "/site-lisp/snippets")
                         (concat mcgemacs-root-dir "/site-lisp/extensions/snippets/yasnippet-snippet/snippets")
                         )))
              (dolist (dir custom-snippet-dirs)
                (add-to-list 'yas-snippet-dirs dir)))
            (yas-reload-all)
            (yas-minor-mode 1)

            ;; 禁用某些模式下的 yasnippet
            (dolist (hook '(term-mode-hook))
              (add-hook hook (lambda () (yas-minor-mode -1))))
            ))

(provide 'init-yasnippet)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here
