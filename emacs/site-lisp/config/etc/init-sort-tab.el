;;; init-sort-tab.el -- Config for Tab -*- lexical-binding: t; -*-

;; Filename: init.el
;; Description: Config for Tab
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:25>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Tab
;;

;;; Installation:
;;
;; Put init-sort-tab.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-sort-tab)
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
(require 'sort-tab)

;;; Code:

(sort-tab-mode 1)

(setq sort-tab-hide-function
      (lambda (buf)
        (with-current-buffer buf
          (or (derived-mode-p 'dired-mode) ; 隐藏 dired-mode 缓冲区
              (derived-mode-p 'eshell-mode) ; 隐藏 aweshell 缓冲区
              ;; 可以添加更多条件来隐藏其他缓冲区
              ))))

(provide 'init-sort-tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-sort-tab.el ends here
