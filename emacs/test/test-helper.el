;;; test-helper.el --- Shared configuration test setup -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defconst mcg-test-root
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name)))
  "Repository Emacs directory, independent of the calling directory.")

(dolist (relative '("core" "modules/editor" "modules/completion"
                    "modules/tools" "modules/enhance" "modules/org" "modules/ui"))
  (add-to-list 'load-path
               (expand-file-name relative
                                 (expand-file-name "site-lisp/config/" mcg-test-root))))

(defvar mcg-emacs-dir mcg-test-root)
(defvar mcg-extensions-dir (expand-file-name "site-lisp/extensions" mcg-test-root))
(defvar mcg-org-directory (expand-file-name "org" temporary-file-directory))
(defvar mcg-org-agenda-directory (expand-file-name "agenda" mcg-org-directory))
(defvar mcg-org-notes-directory (expand-file-name "notes" mcg-org-directory))
(defvar mcg-org-default-notes-file (expand-file-name "notes.org" mcg-org-notes-directory))
(defvar mcg-org-task-file (expand-file-name "tasks.org" mcg-org-agenda-directory))

(require 'core-modules)
(require 'init-org-capture)

(defun mcg-test-capture-template (key)
  "Return capture template matching KEY."
  (assoc key org-capture-templates))

(defun mcg-test-capture-template-prop (key prop)
  "Return PROP from capture template KEY."
  (plist-get (nthcdr 5 (mcg-test-capture-template key)) prop))

(provide 'test-helper)
