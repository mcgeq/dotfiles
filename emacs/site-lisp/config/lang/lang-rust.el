;;; lang-rust.el -- Config for Rust -*- lexical-binding: t; -*-

;; Filename: lang-rust.el
;; Description: Config for Rust
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: 2025-01-04 15:46:46
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Rust
;;

;;; Installation:
;;
;; Put lang-rust.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lang-rust)
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
(require 'rust-mode)
;;; Code:

(with-eval-after-load 'rust-ts-mode
  (lazy-load-set-keys
   '(
     ("C-c C-u C-k" . rust-check)
     ("C-c C-u C-l" . rust-run-clippy)
     ("C-c C-u C-r" . rust-run)
     ("C-c C-u C-u" . rust-compile)
     ("C-c C-u C-t" . rust-test)
     )
   rust-ts-mode-map)
  )

(provide 'lang-rust)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-rust.el ends here
