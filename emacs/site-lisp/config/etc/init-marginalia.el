;;; init-marginalia.el -- Config for Marginalia -*- lexical-binding: t; -*-

;; Filename: init-marginalia.el
;; Description: Config for Marginalia
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:37>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Marginalia
;;

;;; Installation:
;;
;; Put init-marginalia.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-marginalia)
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
(require 'marginalia)

;;; Code:

;; Display the purpose and comment of each command in minibuffer
(add-hook 'after-init-hook 'marginalia-mode)
(setq marginalia-mode t)

(provide 'init-marginalia)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-marginalia.el ends here
