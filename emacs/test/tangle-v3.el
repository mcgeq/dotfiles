;;; tangle-v3.el --- Tangle V3 config files -*- lexical-binding: t; -*-

;;; Commentary:
;; Script to tangle V3 configuration org files

;;; Code:

(require 'org)
(require 'ob-tangle)

(setq org-confirm-babel-evaluate nil)

(defvar tangle-v3-root-dir
  (expand-file-name ".." (file-name-directory load-file-name))
  "Root directory of emacs config.")

(defvar tangle-v3-source-dir
  (expand-file-name "config-org-v3" tangle-v3-root-dir)
  "V3 source directory.")

(defvar tangle-v3-target-dir
  (expand-file-name "site-lisp/config" tangle-v3-root-dir)
  "V3 target directory for tangled files.")

(defun tangle-v3-file (org-file target-subdir)
  "Tangle ORG-FILE to TARGET-SUBDIR."
  (let* ((target-dir (expand-file-name target-subdir tangle-v3-target-dir))
         (org-path (expand-file-name org-file tangle-v3-source-dir))
         (el-name (concat (file-name-sans-extension (file-name-nondirectory org-file)) ".el"))
         (el-path (expand-file-name el-name target-dir))
         (source-el-path (expand-file-name el-name (file-name-directory org-path))))
    ;; Ensure target directory exists
    (unless (file-directory-p target-dir)
      (make-directory target-dir t))
    ;; Tangle the file
    (when (file-exists-p org-path)
      (message "Tangling %s -> %s" org-path el-path)
      ;; Tangle to source directory first (due to relative tangle paths in org files)
      (org-babel-tangle-file org-path nil "emacs-lisp")
      ;; Copy to target directory
      (when (file-exists-p source-el-path)
        (copy-file source-el-path el-path t))
      (message "Done tangling %s" org-file))))

(defun tangle-v3-core ()
  "Tangle all core V3 files."
  (tangle-v3-file "core/core-bootstrap.org" "core")
  (tangle-v3-file "core/core-lib.org" "core")
  (tangle-v3-file "core/core-modules.org" "core"))

(defun tangle-v3-ui ()
  "Tangle all UI V3 module files."
  (tangle-v3-file "modules/ui/+theme.org" "modules/ui")
  (tangle-v3-file "modules/ui/+modeline.org" "modules/ui"))

(defun tangle-v3-lang ()
  "Tangle all lang V3 module files."
  (tangle-v3-file "modules/lang/+lsp.org" "modules/lang"))

(defun tangle-v3-all ()
  "Tangle all V3 files."
  (tangle-v3-core)
  (tangle-v3-ui)
  (tangle-v3-lang))

;; Run if called as script
(when noninteractive
  (tangle-v3-all)
  (message "V3 files tangled successfully!"))

(provide 'tangle-v3)
;;; tangle-v3.el ends here
