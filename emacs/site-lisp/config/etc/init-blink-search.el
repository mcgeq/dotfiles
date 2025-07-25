;;; init-blink-search.el -- Config for blink search -*- lexical-binding: t; -*-

;; Filename: init-blink-search.el
;; Description: Config for blink search
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:11>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for blink search
;;

;;; Installation:
;;
;; Put init-blink-search.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-blink-search)
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
(require 'blink-search)

;;; Code:

(setq blink-search-common-director '(("REPO" (concat mcgemacs-root-dir "/site-lisp/extensions"))
                                       ("CONFIG" mcgemacs-config-dir)
                                       ("BLOG" mcgemacs-blog-org-dir)))

(provide 'init-blink-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-blink-search.el ends here
