;;; init.el --- MCG Emacs top-level entrypoint -*- lexical-binding: t; -*-

;;; Commentary:
;; This file makes the emacs/ directory usable directly as ~/.emacs.d.
;; The actual framework initializer lives in site-lisp/config/init.el.

;;; Code:

(defvar mcg-root-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the MCG Emacs distribution.")

(load (expand-file-name "site-lisp/config/init.el" mcg-root-dir)
      nil 'nomessage)

;;; init.el ends here
