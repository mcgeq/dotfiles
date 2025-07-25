;;; init-vundo.el -- Config for Vundo -*- lexical-binding: t; -*-

;; Filename: init-vundo.el
;; Description: Config for Vundo
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:17>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Vundo
;;

;;; Installation:
;;
;; Put init-vundo.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-vundo)
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
(require 'vundo)

;;; Code:
(defhydra hydra-vundo (:color pink :hint nil)
  "
  _l_ forward       _n_ext       _f_ confirm
  _h_ backward      _p_revious   _i_ inspect
  _j_ stem root     _d_ debug    _Q_ quit
  _k_ stem end      _,_ goto last saved
  "
  ("l"  vundo-forward)
  ("h"  vundo-backward)
  ("n"  vundo-next)
  ("p"  vundo-previous)
  ("j"  vundo-stem-root)
  ("k"  vundo-stem-end)
  (","  vundo-goto-last-saved)
  ("Q"  vundo-quit)
  ;("C-g"  vundo-quit)
  ("f"  vundo-confirm)
  ;("C-m"  vundo-confirm)
  ("i"  vundo--inspect)
  ("d"  vundo--debug)
  ("q" nil)
  ("." nil :color blue)
  )

(define-key vundo-mode-map "." 'hydra-vundo/body)

(provide 'init-vundo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vundo.el ends here
