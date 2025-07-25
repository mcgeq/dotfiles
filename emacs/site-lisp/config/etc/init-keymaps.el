;;; init-keymaps.el -- Config for Keymaps -*- lexical-binding: t; -*-

;; Filename: init-keymaps.el
;; Description: Config for Emacs Start
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:04>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Keymaps
;;

;;; Installation:
;;
;; Put init-keymaps.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-keymaps)
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
(require 'transient)
(require 'markmacro)
(require 'org)

;;; Code:
;; 解除不常用的快捷键定义
 (lazy-load-unset-keys
  '("C-z" "s-q" "M-m" "M-l"))
   ;; replace-string
   (global-set-key (kbd "C-c r") 'replace-string)
   ;; make-directory
   (global-set-key (kbd "C-c d") 'make-directory)

(global-set-key (kbd "C-q") 'mcg/open-init-file)

;; org download image
(lazy-load-local-keys
 '(("C-c v" . mg/org-insert-clipboard-image))
 org-mode-map
 "init-org-download")

;; C-a
;; 将新函数绑定到 Ctrl+a
(global-set-key (kbd "C-a") 'mcge-smart-move-beginning-of-line)

;; insert newline
(global-set-key (kbd "C-c <down>")
                (lambda ()
                  (interactive)
                  (end-of-line)
                  (newline-and-indent)))
(global-set-key (kbd "C-c <up>")
                (lambda ()
                  (interactive)
                  (beginning-of-line)
                  (newline)
                  (forward-line -1)
                  (indent-according-to-mode)))
(global-set-key (kbd "M-m p n")
                (lambda ()
                  (interactive)
                  (forward-line 1)
                  (transpose-lines 1)
                  (forward-line -1)
                  (indent-according-to-mode)))
;; move current line to prev or next line
(global-set-key (kbd "M-m p p")
                (lambda ()
                  (interactive)
                  (transpose-lines 1)
                  (forward-line -2)
                  (indent-according-to-mode)))
;; recentf
(global-set-key (kbd "C-c C-r")   'recentf)

(defun previous-multilines ()
  "Scroll down multiple lines"
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun next-multilines ()
  "Scroll up multiple lines"
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(global-set-key (kbd "M-n") 'next-multilines)
(global-set-key (kbd "M-p") 'previous-multilines)

;;; init-magit.el
;; magit
(lazy-load-global-keys
 '(
   ("M-m s t" . magit-status)
   ("M-m s a" . magit-submodule-add)
   ("M-m s r" . magit-submodule-remove)
   ("M-m f r" . magit-file-rename)
   ("M-m b r" . magit-branch-rename)
   ("M-m b c" . magit-branch-create)
   ("M-m r r" . magit-remote-rename)
   ("M-m l"   . magit-log)
   ("M-m d d" . magit-dispatch)
   ("M-m d f" . magit-file-dispatch)
   ("M-m p f" . magit-pull)
   ("M-m p h" . magit-push)
   ("M-m m"   . mcge-magit-menu)
   )
 "init-magit")

;;; ### Sort-Tab ### from lazycat
;;; --- 多标签浏览
(lazy-load-global-keys
 '(
   ("M-7" . sort-tab-select-prev-tab)    ;选择前一个标签
   ("M-8" . sort-tab-select-next-tab)    ;选择后一个标签
   ("M-s 7" . sort-tab-select-first-tab) ;选择第一个标签
   ("M-s 8" . sort-tab-select-last-tab)  ;选择最后一个标签
   ("C-;" . sort-tab-close-current-tab)  ;关闭当前标签
   ("M-s q" . sort-tab-close-other-tabs)   ;关闭后台标签
   ("M-s Q" . sort-tab-close-all-tabs)     ;关闭所有标签
   )
 "sort-tab")

(global-set-key (kbd "C-c C-n") 'yas-new-snippet)

;; elisp-mode
;;; Elisp
;;; --- Elisp 编程设置
(let ((map emacs-lisp-mode-map))
  (define-key map (kbd "C-c C-b") 'eval-buffer)
  (define-key map (kbd "C-c C-c") 'eval-to-comment))
(let ((map lisp-interaction-mode-map))
  (define-key map (kbd "C-c C-c") 'eval-to-comment))
;;(let ((map org-mode-map))
;;  (define-key map (kbd "C-c C-;") 'eval-to-comment))

;;; ### vundo ###
;;; --- 可视化撤销插件
(lazy-load-global-keys
 '(
   ("C-/" . undo)
   ("M-]" . vundo)
   )
 "init-vundo")

;; org-capture

(lazy-load-global-keys
 '(
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   )
 "init-capture-hugo")

(lazy-load-set-keys
 '(("C-c e" . org-edit-src-code)
   )
 org-mode-map
 )

;; embark

(lazy-load-global-keys
 '(
   ("C-."     . embark-act)
   ("C-,"     . embark-dwim)
   ("C-h b"   . embark-bindings)
   ("C-h B"   . embark-bindings-at-point)
   ("C-x n"   . embark-next-symbol)
   ("C-x p"   . embark-previous-symbol)
   ("C-x j"   . consult-mark)
   ("C-c g"   . consult-ripgrep)
   ("C-c f"   . consult-find)
   ("C-c b"   . consult-buffer)
   ("C-c n h" . mcg/consult-find-org-headings)
   ("C-s"     . consult-line)
   ("M-s i"   . consult-imenu)
     )
  "init-embark")

;;; --- 代码语法补全
(global-set-key (kbd "M-g d")  'lsp-bridge-find-def)
(global-set-key (kbd "M-g o")  'lsp-bridge-find-def-other-window)
(global-set-key (kbd "M-g c")  'lsp-bridge-popup-documentation)
(global-set-key (kbd "M-g D")    'lsp-bridge-find-def-return)
(global-set-key (kbd "M-g l")  'lsp-bridge-find-impl)
(global-set-key (kbd "M-g r")    'lsp-bridge-find-references)
(global-set-key (kbd "M-g n")    'lsp-bridge-rename)
(global-set-key (kbd "M-g j n")  'lsp-bridge-diagnostic-jump-next)
(global-set-key (kbd "M-g j p")  'lsp-bridge-diagnostic-jump-prev)
(global-set-key (kbd "M-g <up>") 'lsp-bridge-popup-documentation-scroll-up)
(global-set-key (kbd "M-g <down>") 'lsp-bridge-popup-documentation-scroll-down)

;; ### Blink Search ###
;;; --- 最快的搜索框架
(lazy-load-global-keys
 '(
   ("C-S-y" . blink-search)
   )
 "init-blink-search")

;;; ### Markmacro ###
;;; --- 标记对象的键盘宏操作
(lazy-load-global-keys
 '(
   ("M-m c s" . markmacro-rect-set)          ;记录矩形编辑开始的位置
   ("M-m c d" . markmacro-rect-delete)       ;删除矩形区域
   ("M-m c r" . markmacro-rect-replace)      ;替换矩形区域的内容
   ("M-m c i" . markmacro-rect-insert)       ;在矩形区域前插入字符串
   ("M-m c m" . markmacro-rect-mark-columns) ;转换矩形列为标记对象
   ("M-m c S" . markmacro-rect-mark-symbols) ;转换矩形列对应的符号为标记对象
   ("M-m c a" . markmacro-apply-all)         ;应用键盘宏到所有标记对象
   ("M-m c e" . markmacro-apply-all-except-first) ;应用键盘宏到所有标记对象, 除了第一个， 比如下划线转换的时候
   )
 "init-markmacro")

;;; ### Color-Rg ###
;;; --- 搜索重构
(lazy-load-global-keys
 '(
   ("M-m g g" . color-rg-search-symbol)
   ("M-m g h" . color-rg-search-input)
   ("M-m g j" . color-rg-search-symbol-in-project)
   ("M-m g k" . color-rg-search-input-in-project)
   ("M-m g ," . color-rg-search-symbol-in-current-file)
   ("M-m g ." . color-rg-search-input-in-current-file)
   )
 "color-rg")

(provide 'init-keymaps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-keymaps.el ends here
