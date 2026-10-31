;;; round-two-startup-smoke.el --- Real startup binding check -*- lexical-binding: t; -*-

;; Run from emacs/: emacs -Q --batch -l test/round-two-startup-smoke.el
(require 'cl-lib)
(load (expand-file-name "../scripts/profile-startup.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(unless (eq (lookup-key global-map (kbd "C-c s b")) 'blink-search)
  (error "Startup lost the lazy Blink Search binding"))
(unless (eq (lookup-key global-map (kbd "C-c s g")) 'color-rg-search-input)
  (error "Startup lost the lazy Color-rg binding"))

(cl-letf (((symbol-function 'consult-compile-error)
           (lambda () (interactive) 'smoke-ok))
          ((symbol-function 'next-error)
           (lambda () (interactive) 'smoke-ok)))
  (unless (eq (call-interactively 'mcg-compile-errors) 'smoke-ok)
    (error "First-use compile dispatch failed")))
(unless (mcg-module-loaded-p :tools 'compile)
  (error "First use did not record the compile module"))
(when mcg-module-load-errors
  (error "Startup module errors: %S" mcg-module-load-errors))
(princ "Startup key preservation and first-use compile smoke: passed\n")
