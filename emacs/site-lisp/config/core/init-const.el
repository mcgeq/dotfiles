;;; init-const.el -- Config for Const -*- lexical-binding: t; -*-

;; Filename: init-const.el
;; Description: Config for Const
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: 2025-01-04 15:46:46
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Const
;;

;;; Installation:
;;
;; Put init-const.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-const)
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

(defconst *is-win32p*
  (memq system-type '(cygwin windows-nt ms-dos))
  "Running on a WinNT System.")

(defconst *is-linux*
  (eq system-type 'gnu/linux)
  "Running on a GNU/Linux System.")

(defconst *is-mac*
  (eq system-type 'darwin)
  "Running on a MacOS System.")

;; GUI `window-system'
(defconst *is-gui-p*
  (memq window-system '(x w32 ns pc pgtk))
  "Currently running in terminal mode.")

(defvar mcg--fonts-default '("FiraCode Nerd Font Mono","Cascadia Mono" "FiraCode Nerd Font" "DejaVuSansMono Nerd Font Mono" "Consolas" "Source Code Pro" "Hack" "Fira Code"))
(defvar mcg--fonts-unicode '("Symbola" "Segoe UI Symbol" "Symbol"))
(defvar mcg--fonts-emoji    '("Noto Color Emoji" "Apple Color Emoji"))
(defvar mcg--fonts-cjk     '("KaiTi" "WenQuanYi Micro Hei" "Microsoft Yahei UI" "Microsoft Yahei" "STFangsong"))
(defvar mcgemacs-custom-icon-font-family "FiraCode Nerd Font")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
"Emacs is 29 or above.")

;; 定义 emacs 配置文件目录上级目录
(defvar mcgemacs-custom-dir (if *is-win32p* (file-truename "D:/config")
                              (file-truename "~")))
;; 自定义博客目录
(defvar mcgemacs-blog-dir      (if *is-win32p* (file-truename "D:/workspaces/blog/mcge-blog")
                                 (file-truename "~/workspaces/blog/mcge-blog")))

(defvar mcgemacs-blog-org-dir  (concat mcgemacs-blog-dir "/content-org"))
;; 自定义 org 目录
(defvar mcgemacs-org-dir       (if *is-win32p* (file-truename "D:/org")
                                 (file-truename "~/org")))
;; 定义一些启动目录，方便下次迁移
(defvar mcgemacs-root-dir      (concat mcgemacs-custom-dir "/dotfiles/emacs"))
(defvar mcgemacs-config-dir    (concat mcgemacs-root-dir "/config-org"))
(defvar mcgemacs-extension-dir (concat mcgemacs-root-dir "/extensions"))

;; custom lsp-bridge
(defvar mcgemacs-custom-lsp-bridge-langserver-dir (concat mcgemacs-root-dir "/langservers"))
(defvar mcgemacs-custom-lsp-bridge-multiserver-dir (concat mcgemacs-root-dir "/multiservers"))

(defvar mcgemacs-org-agenda-files (concat mcgemacs-org-dir "/diary-by-months/"))
(defvar mcgeamcs-org-agenda-diary-file (expand-file-name "standard-diary"
                                                         (concat mcgemacs-org-dir "/mcge/")))

(defvar org-file-note     (expand-file-name "notes.org"
                                            (concat mcgemacs-org-dir "/notes")))
(defvar org-file-task     (expand-file-name "tasks.org"
                                            mcgemacs-org-dir))
(defvar org-file-calendar (expand-file-name "calendar.org"  mcgemacs-org-dir))
(defvar org-file-finished (expand-file-name "finished.org"  mcgemacs-org-dir))
(defvar org-file-canceled (expand-file-name "canceled.org"  mcgemacs-org-dir))
(defvar org-file-diary    (expand-file-name (format-time-string "diary_%Y%m.org")
                                            mcgemacs-org-agenda-files))
(defvar org-file-journal  (expand-file-name "inbox.org"
                                            (concat mcgemacs-org-dir "/journal")))
(defvar org-file-billing  (expand-file-name "billing.org"
                                            (concat mcgemacs-org-dir "/billing")))
(defvar org-file-password (expand-file-name "passwords.org.cpt"
                                            (concat mcgemacs-org-dir "/private")))
(defvar org-file-blog   (expand-file-name "all-posts.org" mcgemacs-blog-org-dir))
;; custom
(defvar org-emacs-blog   (expand-file-name "all-about-emacs.org" mcgemacs-blog-org-dir))
(defvar org-prattle-blog   (expand-file-name "all-about-prattle.org" mcgemacs-blog-org-dir))
(defvar org-tools-blog   (expand-file-name "all-about-tools.org" mcgemacs-blog-org-dir))
(defvar org-development-blog   (expand-file-name "all-about-development.org"
                                                 mcgemacs-blog-org-dir))
(defvar org-mcu-blog     (expand-file-name "all-about-mcu-blog.org" mcgemacs-blog-org-dir))

;; image size
(defvar mcge/image-scale-mode-step 1.2
    "Image scale factor.")

;; emacs-rime user data
(defvar mcgemacs-rime-user-data-dir (if *is-linux* (expand-file-name "~/.config/fcitx/rime")
                                      (expand-file-name (concat mcgemacs-custom-dir "/rime"))))

(provide 'init-const)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
