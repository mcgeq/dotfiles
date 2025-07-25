;;; init-ui.el -- Config for UI -*- lexical-binding: t; -*-

;; Filename: init.el
;; Description: Config for UI
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
;; Config for UI
;;

;;; Installation:
;;
;; Put init-ui.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-ui)
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
(require 'ef-themes)

;;; Code:


;; Automatically switch themes
;; (if (>= (string-to-number (format-time-string "%H")) 18)
;;     (load-theme 'ef-bio t)
;;   (load-theme 'ef-summer t))
;; ef-arbutus
;; ef-tritanopia-light
(load-theme 'ef-arbutus t)

;; nerd-icons
(require 'nerd-icons)
(setq nerd-icons-font-family (concat mcgemacs-custom-icon-font-family ""))

(require 'nerd-icons-completion)
(nerd-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

(provide 'init-ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
