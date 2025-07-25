;;; init-awesome-tray.el -- Config for modeline -*- lexical-binding: t; -*-

;; Filename: init.el
;; Description: Config for modeline
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:38>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for modeline
;;

;;; Installation:
;;
;; Put init-awesome-tray.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-awesome-tray)
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
(require 'awesome-tray)

;;; Code:
(setq awesome-tray-position 'center)
(setq awesome-tray-date-format "[%-Y-%-m-%-d %-H:%-M]")
(setq awesome-tray-active-modules '("git" "location" "belong" "mode-name" "date"))
(setq awesome-tray-info-padding-right 1)
(setq awesome-tray-second-line t)
(unless (display-graphic-p)
  (setq-default mode-line-format nil))
(awesome-tray-mode 1)

(provide 'init-awesome-tray)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-awesome-tray.el ends here
