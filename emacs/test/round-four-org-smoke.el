;;; round-four-org-smoke.el --- Isolated Org initialization smoke -*- lexical-binding: t; -*-
(require 'cl-lib)
(let* ((mcg-test-root (expand-file-name "../" (file-name-directory load-file-name)))
       (storage (make-temp-file "mcg-org-smoke" t)))
  (add-to-list 'load-path (expand-file-name "site-lisp/config/core" mcg-test-root))
  (require 'core-lib)
  (mcg-init-paths mcg-test-root)
  (mcg-setup-load-path)
  (unwind-protect
      (progn
        (when (featurep 'org) (error "Core initialization eagerly loaded Org"))
        (mcg-org-set-storage-root storage)
        (setq org-capture-templates '(("z" "Private template")))
        (require 'init-org)
        (unless (assoc "z" org-capture-templates) (error "Lost private template"))
        (when (featurep 'ox-gfm) (error "Loaded GFM before export"))
        (with-temp-buffer
          (org-mode)
          (unless (local-variable-p 'system-time-locale) (error "Locale is not local")))
        (require 'ox)
        (unless (featurep 'init-ox-gfm) (error "Export backend setup was not triggered"))
        (princ "Org initialization, private capture and lazy export smoke: passed\n"))
    (delete-directory storage t)))
