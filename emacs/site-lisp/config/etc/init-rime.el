;;; init-rime.el -- Config for rime -*- lexical-binding: t; -*-

;; Filename: init-rime.el
;; Description: Config for rime
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:14>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for rime
;;

;;; Installation:
;;
;; Put init-rime.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-rime)
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
;;  from https://www.hugchange.life/posts/202401_core_emacs_config.html

;;; Require:
(require 'rime)
(require 'popup)

;;; Code:

;; 设置默认输入法为 rime
(setq default-input-method "rime")

;; 设置 rime 用户数据目录
(setq rime-user-data-dir "~/.config/emacs_rime")

;; 设置 rime 键位映射
(define-key rime-active-mode-map (kbd "M-DEL") 'rime--escape)
(define-key rime-active-mode-map (kbd "C-w") 'rime--escape)

;; 配置 rime 输入法切换
;; 手动切换 rime 输入法的函数
(defun rime-switch-manually ()
  (interactive)
  (rime-force-enable))

;; 自动切换 rime 输入法的函数
(defun rime-switch-auto ()
  (interactive)
  (rime-disable))

;; 修改 rime 的颜色设置
(defun rime-color-change ()
  (interactive)
  (setq rime-show-candidate 'posframe))

;; 同步 ibus 和 emacs-rime 用户数据库
(defun sync-ibus-and-emacs-rime-userdb ()
  (interactive)
  (rime-sync)
  (start-process-shell-command
   ""
   nil
   "ibus exit;cd ~/.config/ibus/rime; rime_dict_manager -s;ibus-daemon --xim -d -r")
  (message "ibus-rime and emacs rime sync done"))

(setq rime-show-candidate 'posframe)

(setq rime-posframe-properties
          (list :background-color "#333333"
                :foreground-color "#dcdccc"
                :internal-border-width 10))

;; 手动切换中英文
(defun +translate-symbol-to-rime ()
  (interactive)
  (let* ((end (point))
         (beg (+ end (skip-chars-backward "a-z")))
         (input (buffer-substring beg end))
         )
    (delete-region beg end)
    (if current-input-method nil (toggle-input-method))
    (dolist (c (string-to-list input))
      (rime-lib-process-key c 0)
      )
    (rime--redisplay)))

(defun convert-code-or-disable-rime ()
  (interactive)
  (if (not (featurep 'ecb))
      (require 'pyim)
      )
  (let ((str-before-1 (pyim-char-before-to-string 0)))
    (cond ((pyim-string-match-p "[a-z]" str-before-1)
           (+translate-symbol-to-rime))
          ((pyim-string-match-p "[[:punct:]]\\|：" str-before-1)
           (pyim-punctuation-translate-at-point))
          (t (toggle-input-method)))))

;; 根据上下文自动切换中英文
(setq rime-disable-predicates
      '(
        rime-predicate-after-ascii-char-p
        rime-predicate-evil-mode-p
        rime-predicate-punctuation-after-space-cc-p
        rime-predicate-punctuation-after-ascii-p
        rime-predicate-punctuation-line-begin-p
        rime-predicate-org-in-src-block-p
        rime-predicate-prog-in-code-p
        rime-predicate-org-latex-mode-p
        ;; rime-predicate-space-after-ascii-p
        rime-predicate-space-after-cc-p
        rime-predicate-current-uppercase-letter-p
        rime-predicate-tex-math-or-command-p
        ))

;; 光标颜色自动变化
;; 在输入中文的时候光标是 input-method-cursor-color 颜色。
(advice-add 'toggle-input-method :after 'change-cursor-color-on-input-method)
(defvar input-method-cursor-color "white"
  "Default cursor color if using an input method.")

(defun get-frame-cursor-color ()
  "Get the cursor-color of current frame."
  (interactive)
  (frame-parameter nil 'cursor-color))

(defvar default-cursor-color (get-frame-cursor-color)
  "Default text cursor color.")

(defun change-cursor-color-on-input-method ()
  "Set cursor color depending on whether an input method is used or not."
  (interactive)
  (set-cursor-color (if (and (rime--should-enable-p)
                             (not (rime--should-inline-ascii-p))
                             current-input-method)
                        input-method-cursor-color
                      default-cursor-color)))

(add-hook 'post-command-hook 'change-cursor-color-on-input-method)

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
