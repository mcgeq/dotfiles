;;; init-org-numbering.el -- Config for org-numbering -*- lexical-binding: t; -*-

;; Filename: init-org-numbering.el
;; Description: Config for org-numbering
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
;; Config for org-numbering
;;

;;; Installation:
;;
;; Put init-org-numbering.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-org-numbering)
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
(require 'org-numbering)

;;; Code:

(setq org-numbering-level-scheme
      '((1 . ((scheme . decimal)      ; (scheme . decimal) 表示十进制编号
              (combine . nil)))       ; <= `nil` 表示不组合
        (2 . ((scheme . decimal)      ; 1.1
              (combine . t)))         ; <= `t` 表示组合
        (3 . ((scheme . decimal)      ; 1.1.1
              (combine . t)))         ; <= `t` 表示组合
        (4 . ((scheme . alpha)        ; a)
              (combine . nil)))
        (5 . ((scheme . paren-num)    ; (1)
              (combine . nil)))))

(provide 'init-org-numbering)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-numbering.el ends here
