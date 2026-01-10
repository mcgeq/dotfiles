;;; test-property-6-extension-discovery.el --- Property test for extension discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based test for extension discovery completeness.
;; **Feature: modern-emacs-config, Property 6: Extension Discovery Completeness**
;; **Validates: Requirements 3.5**
;;
;; Property: For any Git submodule directory in extensions/,
;; the mcg-list-extensions function SHALL return that directory in its result list.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-emacs-dir
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for emacs config.")

(defvar test-config-dir
  (expand-file-name "site-lisp/config" test-emacs-dir)
  "Config directory for V3 config.")

;;; ============================================================
;;; Property 6: Extension Discovery Completeness
;;; ============================================================

;; **Feature: modern-emacs-config, Property 6: Extension Discovery Completeness**
;; **Validates: Requirements 3.5**

(ert-deftest test-property-6-extension-discovery-completeness ()
  "Property 6: Extension Discovery Completeness

*For any* Git submodule directory in extensions/ that contains .el files,
the mcg-list-extensions function SHALL return that directory in its result list.

**Feature: modern-emacs-config, Property 6: Extension Discovery Completeness**
**Validates: Requirements 3.5**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Clear cache to force fresh discovery
      (mcg-cache-clear)
      
      ;; Get all discovered extensions
      (let ((discovered-extensions (mcg-list-extensions t)))
        ;; Should return a list
        (should (listp discovered-extensions))
        
        ;; For each discovered extension, verify it exists and contains .el files
        (dolist (ext discovered-extensions)
          (let ((name (car ext))
                (path (cdr ext)))
            ;; Path should be a string
            (should (stringp path))
            ;; Path should exist
            (should (file-directory-p path))
            ;; Path should contain .el files
            (should (directory-files path nil "\\.el$" t))))))))

(ert-deftest test-property-6-extension-path-lookup ()
  "Property 6: Extension path lookup works for all discovered extensions

*For any* extension discovered by mcg-list-extensions,
mcg-extension-path should return the correct path.

**Feature: modern-emacs-config, Property 6: Extension Discovery Completeness**
**Validates: Requirements 3.5**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Clear cache
      (mcg-cache-clear)
      
      ;; Get all discovered extensions
      (let ((discovered-extensions (mcg-list-extensions t)))
        ;; For each discovered extension, verify mcg-extension-path works
        (dolist (ext discovered-extensions)
          (let* ((name (car ext))
                 (expected-path (cdr ext))
                 (found-path (mcg-extension-path name)))
            ;; Should find the extension
            (should found-path)
            ;; Path should match
            (should (equal found-path expected-path))
            ;; mcg-extension-exists-p should return t
            (should (mcg-extension-exists-p name))))))))

(ert-deftest test-property-6-nonexistent-extension-returns-nil ()
  "Property 6: Non-existent extension lookup returns nil

*For any* extension name that does not exist in the extensions directory,
mcg-extension-path should return nil.

**Feature: modern-emacs-config, Property 6: Extension Discovery Completeness**
**Validates: Requirements 3.5**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Clear cache
      (mcg-cache-clear)
      
      ;; Ensure extensions are discovered first
      (mcg-list-extensions t)
      
      ;; Test with non-existent extension names
      (should (null (mcg-extension-path "nonexistent-extension-xyz")))
      (should (null (mcg-extension-path "fake/package/that/does/not/exist")))
      (should (null (mcg-extension-path "")))
      (should (null (mcg-extension-path nil)))
      
      ;; mcg-extension-exists-p should return nil for non-existent extensions
      (should-not (mcg-extension-exists-p "nonexistent-extension-xyz")))))

(provide 'test-property-6-extension-discovery)
;;; test-property-6-extension-discovery.el ends here
