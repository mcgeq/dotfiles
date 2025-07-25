;;; init-markdown.el -- Config for Markdown -*- lexical-binding: t; -*-

;; Filename: init-markdown.el
;; Description: Config for Markdown
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
;; Config for Markdown
;;

;;; Installation:
;;
;; Put init-markdown.el to your load-path.
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

;;; Require:

(require 'markdown-ts-mode)
(require 'flymake-vale)

;;; Code:
(add-hook 'text-mode-hook #'flymake-vale-load)
(add-hook 'latex-mode-hook #'flymake-vale-load)
(add-hook 'org-mode-hook #'flymake-vale-load)
(add-hook 'markdown-mode-hook #'flymake-vale-load)
(add-hook 'message-mode-hook #'flymake-vale-load)

(add-hook 'find-file-hook 'flymake-vale-maybe-load)
;; flymake-vale-modes defaults to:
;;  => (text-mode latex-mode org-mode markdown-mode message-mode)

(add-to-list 'flymake-vale-modes 'adoc-mode)

(add-hook 'org-msg-mode-hook (lambda ()
                               (setq flymake-vale-file-ext "org")
                               (flymake-vale-load)))

(provide 'init-markdown)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-markdown.el ends here
