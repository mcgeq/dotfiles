;;; lang-lua.el -- Config for Lua -*- lexical-binding: t; -*-

;; Filename: lang-lua.el
;; Description: Config for Lua
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: 2025-01-04 15:46:46
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Lua
;;

;;; Installation:
;;
;; Put lang-lua.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lang-lua)
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

(require 'lua-mode)

;;; Code:

(provide 'lang-lua)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-lua.el ends here
