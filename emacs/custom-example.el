;;; custom-example.el --- Example user configuration template -*- lexical-binding: t; -*-

;; Copyright (C) 2024 MCG
;; Author: MCG <mcgeq@outlook.com>
;; Keywords: configuration, example

;;; Commentary:
;;
;; This is an example configuration template for MCG Emacs.
;; It demonstrates common customization options and best practices.
;;
;; Requirements: 13.4
;;
;; Usage:
;;   1. Copy this file to site-lisp/config/private/config.el
;;   2. Uncomment and modify the settings you want to customize
;;   3. Restart Emacs or run M-x mcg-reload-config
;;
;; Note: The private config file is NOT tracked by Git, so your
;; personal settings remain private.

;;; Code:

;;; ============================================================
;;; Quick Start - Essential Settings
;;; ============================================================

;; Uncomment and modify these for a quick start:

;; Your base directory for personal files
(setq mcg-user-base-dir "D:/config")

;; Font settings
(setq mcg-font-family "FiraCode Nerd Font Mono")
(setq mcg-font-size 14)

;; Enable features
(setq mcg-enable-line-numbers t)
(setq mcg-enable-auto-save t)


;;; ============================================================
;;; Path Configuration
;;; ============================================================

;; Base directory for your personal files
;; Windows: "D:/config"
;; Linux/macOS: (expand-file-name "~")
;; (setq mcg-user-base-dir "D:/config")

;; Org files directory
;; (setq mcg-org-dir (expand-file-name "org" mcg-user-base-dir))

;; Blog/Hugo directory for ox-hugo
;; (setq mcg-blog-dir (expand-file-name "workspaces/blog" mcg-user-base-dir))


;;; ============================================================
;;; Theme and Appearance
;;; ============================================================

;; Theme selection
;; ef-themes: 'ef-day 'ef-night 'ef-spring 'ef-summer 'ef-autumn 'ef-winter
;;            'ef-duo-dark 'ef-duo-light 'ef-trio-dark 'ef-trio-light
;; modus-themes: 'modus-operandi 'modus-vivendi
;; (setq mcg-theme 'ef-day)

;; Override theme after loading
;; (with-eval-after-load 'init-theme
;;   (load-theme 'modus-vivendi t))


;;; ============================================================
;;; Font Configuration
;;; ============================================================

;; ASCII font family (monospace recommended)
;; Popular choices:
;;   "FiraCode Nerd Font Mono"
;;   "JetBrains Mono"
;;   "Cascadia Code"
;;   "Source Code Pro"
;; (setq mcg-font-family "FiraCode Nerd Font Mono")

;; Font size (in points)
;; (setq mcg-font-size 14)

;; CJK (Chinese/Japanese/Korean) font
;; Windows: "Microsoft YaHei" or "SimHei"
;; macOS: "PingFang SC" or "Hiragino Sans GB"
;; Linux: "Noto Sans CJK SC" or "WenQuanYi Micro Hei"
;; (setq mcg-cjk-font "Microsoft YaHei")


;;; ============================================================
;;; Editor Defaults
;;; ============================================================

;; Line numbers
;; (setq mcg-enable-line-numbers t)
;; (setq display-line-numbers-type 'relative)  ; 'relative 'absolute nil

;; Indentation
;; (setq-default indent-tabs-mode nil)  ; Use spaces
;; (setq-default tab-width 4)

;; Scrolling behavior
;; (setq scroll-margin 3)
;; (setq scroll-conservatively 101)

;; Auto-save
;; (setq mcg-enable-auto-save t)
;; (setq auto-save-idle 5)  ; seconds


;;; ============================================================
;;; LSP Configuration (lsp-bridge)
;;; ============================================================

;; LSP backend
;; (setq mcg-lsp-backend 'lsp-bridge)

;; Diagnostics
;; (setq lsp-bridge-enable-diagnostics t)
;; (setq lsp-bridge-enable-hover-diagnostic t)

;; Completion
;; (setq acm-enable-icon t)
;; (setq acm-enable-doc t)

;; Language server paths (if not in PATH)
;; (setq lsp-bridge-python-command "python3")
;; (setq lsp-bridge-python-lsp-server "pyright")


;;; ============================================================
;;; Completion System (Vertico)
;;; ============================================================

;; Number of candidates to display
;; (setq vertico-count 15)

;; Enable pinyin search for Chinese
;; (setq mcg-enable-pinyin-search t)

;; Orderless matching style
;; (setq orderless-matching-styles '(orderless-literal orderless-regexp))


;;; ============================================================
;;; Input Method (Rime)
;;; ============================================================

;; Rime user data directory
;; Windows: "D:/config/rime"
;; Linux: "~/.config/fcitx/rime" or "~/.local/share/fcitx5/rime"
;; macOS: "~/Library/Rime"
;; (setq rime-user-data-dir "D:/config/rime")

;; Rime shared data directory (optional)
;; (setq rime-share-data-dir "/usr/share/rime-data")


;;; ============================================================
;;; Org Mode Configuration
;;; ============================================================

;; Enable Org mode
;; (setq mcg-enable-org t)

;; Org directory
;; (setq org-directory "~/org")

;; Agenda files
;; (setq org-agenda-files '("~/org/tasks.org"
;;                          "~/org/projects.org"
;;                          "~/org/calendar.org"))

;; Default notes file
;; (setq org-default-notes-file "~/org/notes.org")

;; Capture templates
;; (setq org-capture-templates
;;       '(("t" "Task" entry
;;          (file+headline "~/org/tasks.org" "Inbox")
;;          "* TODO %?\n  %i\n  %a"
;;          :empty-lines 1)
;;         ("n" "Note" entry
;;          (file+headline "~/org/notes.org" "Notes")
;;          "* %?\n  %i\n  %a"
;;          :empty-lines 1)
;;         ("j" "Journal" entry
;;          (file+datetree "~/org/journal.org")
;;          "* %?\n  %i"
;;          :empty-lines 1)))

;; TODO keywords
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))


;;; ============================================================
;;; Language-Specific Settings
;;; ============================================================

;; Python
;; (setq python-shell-interpreter "python3")
;; (with-eval-after-load 'init-python
;;   (setq python-indent-offset 4))

;; Rust
;; (with-eval-after-load 'init-rust
;;   (setq rust-format-on-save t))

;; TypeScript/JavaScript
;; (setq js-indent-level 2)
;; (setq typescript-indent-level 2)

;; Web mode (HTML/Vue/JSX)
;; (with-eval-after-load 'init-web
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2))

;; C/C++
;; (with-eval-after-load 'init-cpp
;;   (setq c-basic-offset 4))


;;; ============================================================
;;; Git (Magit) Configuration
;;; ============================================================

;; Git executable path (if not in PATH)
;; (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")

;; Default directory for new repositories
;; (setq magit-repository-directories '(("~/projects" . 2)))


;;; ============================================================
;;; Performance Tuning
;;; ============================================================

;; GC threshold (bytes)
;; Default: 16MB, High performance: 100MB
;; (setq gc-cons-threshold (* 100 1024 1024))

;; Restore GC after startup
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq gc-cons-threshold (* 16 1024 1024))))

;; Native compilation (Emacs 29+)
;; (setq native-comp-async-report-warnings-errors nil)
;; (setq native-comp-deferred-compilation t)

;; File name handler optimization
;; (setq file-name-handler-alist nil)  ; Restore in emacs-startup-hook


;;; ============================================================
;;; Debug and Profiling
;;; ============================================================

;; Enable debug mode for verbose logging
;; (setq mcg-debug-mode t)

;; Enable profiling to track module load times
;; (setq mcg-profile-mode t)

;; Show startup time
;; (setq mcg-display-startup-time t)


;;; ============================================================
;;; Custom Keybindings
;;; ============================================================

;; Use mcg-bind-key for conflict detection
;; (mcg-bind-key "C-c g g" #'magit-status "private/config")
;; (mcg-bind-key "C-c f f" #'find-file "private/config")

;; Or use standard global-set-key
;; (global-set-key (kbd "C-c m") #'my-custom-command)

;; Quick access to config files
;; (global-set-key (kbd "C-c e c")
;;                 (lambda ()
;;                   (interactive)
;;                   (find-file (expand-file-name "private/config.el" mcg-config-dir))))


;;; ============================================================
;;; Custom Functions
;;; ============================================================

;; Quick reload configuration
(defun my/reload-config ()
  "Reload MCG Emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" mcg-config-dir))
  (message "Configuration reloaded!"))

;; Open private config
(defun my/open-private-config ()
  "Open private configuration file."
  (interactive)
  (find-file (expand-file-name "private/config.el" mcg-config-dir)))

;; Toggle debug mode
(defun my/toggle-debug-mode ()
  "Toggle MCG debug mode."
  (interactive)
  (setq mcg-debug-mode (not mcg-debug-mode))
  (message "Debug mode: %s" (if mcg-debug-mode "ON" "OFF")))


;;; ============================================================
;;; Module Overrides
;;; ============================================================

;; Override settings after specific modules load
;; Use with-eval-after-load to ensure the module is loaded first

;; Example: Custom theme setup
;; (with-eval-after-load 'init-theme
;;   (load-theme 'modus-vivendi t)
;;   (set-face-attribute 'default nil :background "#1a1a1a"))

;; Example: Extra Rust mode hooks
;; (with-eval-after-load 'init-rust
;;   (add-hook 'rust-mode-hook #'my-rust-setup))

;; Example: Custom Org mode settings
;; (with-eval-after-load 'init-org
;;   (setq org-hide-emphasis-markers t)
;;   (setq org-startup-indented t))


;;; ============================================================
;;; Startup Message
;;; ============================================================

(message "✅ Custom configuration loaded!")

(provide 'custom-example)
;;; custom-example.el ends here
