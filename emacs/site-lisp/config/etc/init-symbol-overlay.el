;;; init-symbol-overlay.el -- Config for Symbol-overlay -*- lexical-binding: t; -*-

;; Filename: init.el
;; Description: Config for Symbol-overlay
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
;; Config for Symbol-overlay
;;

;;; Installation:
;;
;; Put init-symbol-overlay.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-symbol-overlay)
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
(require 'symbol-overlay)

;;; Code:

(lazy-load-set-keys
 '(
   ("s" . symbol-overlay-put)
   ("n" . symbol-overlay-jump-next)
   ("p" . symbol-overlay-jump-prev)
   ("w" . symbol-overlay-save-symbol)
   ("t" . symbol-overlay-toggle-in-scope)
   ("e" . symbol-overlay-echo-mark)
   ("d" . symbol-overlay-jump-to-definition)
   ("S" . symbol-overlay-isearch-literally)
   ("r" . symbol-overlay-rename)
   ("R" . symbol-overlay-query-replace)
   ("q" . symbol-overlay-remove-all)
   ("<" . symbol-overlay-jump-first)
   (">" . symbol-overlay-jump-last)
   ("M-n" . symbol-overlay-switch-forward)
   ("M-p" . symbol-overlay-switch-backward)
   )
 symbol-overlay-map)

(provide 'init-symbol-overlay)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-symbol-overlay.el ends here
