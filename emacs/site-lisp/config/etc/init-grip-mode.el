;;; init-grip-mode.el -- Config for Grip-Mode -*- lexical-binding: t; -*-

;; Filename: init-grip-mode.el
;; Description: Config for Grip-Mode
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:02>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Grip-Mode
;;

;;; Installation:
;;
;; Put init-grip-mode.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init)
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

(require 'grip-mode)
(require 'flymake-vale)

;;; Code:

;; Make a keybinding: `C-c C-c g'
(define-key markdown-mode-command-map (kbd "g") #'grip-mode)

(setq grip-command 'go-grip)

;; Or start grip when opening a markdown/org buffer
(add-hook 'markdown-mode-hook 'grip-mode)
(add-hook 'org-mode-hook 'grip-mode)

;; Command: auto, grip, go-grip or mdopen
(setq grip-command 'auto)

;; Theme choice
(setq grip-theme 'auto)

;; Use embedded webkit to preview
;; This requires GNU/Emacs version >= 26 and built with the `--with-xwidgets` option.
;; mdopen doesn't support webkit preview.
(setq grip-preview-use-webkit t)

;; You can use this variable to define another browser
;; to use when loading previews. By default this value is `nil`
;; meaning use default browser defined by your system.
;; It respects `grip-preview-use-webkit'.
;; (setq grip-url-browser "")

;; If you want to pass arguements to your custom browser then use
(setq grip-url-args '("arg1" "arg2" "etc"))

;; A base URL to another GitHub API.
;; Only available for `grip'.
(setq grip-github-api-url "")

;; A GitHub username for API authentication
;; Only available for `grip'.
(setq grip-github-user "")

;; A GitHub password or auth token for API auth
;; Only available for `grip'.
(setq grip-github-password "")

;; Preview hostname
;; Only available for `grip'.
(setq grip-preview-host "localhost")

;; When nil, update the preview after file saves only, instead of also
;; after every text change
(setq grip-update-after-change nil)

;; Sleep seconds to ensure the server starts
(setq grip-sleep-time 2)

(provide 'init-grip-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-grip-mode.el ends here
