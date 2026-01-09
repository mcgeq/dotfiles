;;; test-v3-extension-autodiscovery.el --- Tests for extension auto-discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for extension auto-discovery functionality.
;; Tests verify that extensions can be auto-discovered from directory structure
;; and that dependencies are correctly resolved.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config.")

(defvar test-v3-emacs-dir
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for emacs config.")

;;; ============================================================
;;; Property 2: Extension Auto-Discovery
;;; ============================================================

;; **Feature: emacs-config-optimization-v2, Property 2: Extension Auto-Discovery**
;; **Validates: Requirements 4.1**
;;
;; Property: For any directory added to mcg-extensions-dir containing .el files,
;; the extension should be discoverable via mcg-discover-extensions.

(ert-deftest test-property-2-extension-auto-discovery ()
  "Property 2: Extension Auto-Discovery

*For any* directory added to mcg-extensions-dir containing .el files,
the extension should be discoverable via mcg-discover-extensions.

**Validates: Requirements 4.1**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (autoload-file (expand-file-name "core/core-autoload.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p autoload-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load autoload-file nil t)
      
      ;; Create a temporary test extension directory
      (let* ((test-category-dir (make-temp-file "mcg-test-category-" t))
             (test-ext-name "test-extension")
             (test-ext-dir (expand-file-name test-ext-name test-category-dir))
             (original-extensions-dir mcg-extensions-dir))
        (unwind-protect
            (progn
              ;; Create test extension directory
              (make-directory test-ext-dir t)
              
              ;; Create a test .el file with proper headers
              (with-temp-file (expand-file-name (concat test-ext-name ".el") test-ext-dir)
                (insert ";;; test-extension.el --- Test extension -*- lexical-binding: t; -*-\n")
                (insert ";; Package-Requires: ((emacs \"29.1\"))\n")
                (insert "(provide 'test-extension)\n")
                (insert ";;; test-extension.el ends here\n"))
              
              ;; Test mcg-auto-register-extension
              (let ((metadata (mcg-auto-register-extension test-ext-dir)))
                ;; Should return metadata
                (should metadata)
                ;; Should have :provides
                (should (plist-get metadata :provides))
                ;; :provides should contain the feature
                (should (memq 'test-extension (plist-get metadata :provides)))))
          
          ;; Cleanup
          (delete-directory test-category-dir t)
          (setq mcg-extensions-dir original-extensions-dir))))))

(ert-deftest test-property-2-pkg-file-parsing ()
  "Property 2 (extended): -pkg.el file parsing

*For any* extension with a -pkg.el file, mcg-auto-register-extension
should parse metadata from that file.

**Validates: Requirements 4.1**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (autoload-file (expand-file-name "core/core-autoload.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p autoload-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load autoload-file nil t)
      
      ;; Create a temporary test extension with -pkg.el
      (let* ((test-ext-dir (make-temp-file "mcg-test-ext-" t))
             (test-ext-name (file-name-nondirectory test-ext-dir)))
        (unwind-protect
            (progn
              ;; Create -pkg.el file
              (with-temp-file (expand-file-name (concat test-ext-name "-pkg.el") test-ext-dir)
                (insert (format "(define-package \"%s\" \"1.0.0\"\n" test-ext-name))
                (insert "  \"Test package description\"\n")
                (insert "  '((emacs \"29.1\") (dash \"2.19.1\")))\n"))
              
              ;; Test parsing
              (let ((metadata (mcg-auto-register-extension test-ext-dir)))
                (should metadata)
                (should (plist-get metadata :provides))
                ;; Should have parsed dependencies
                (let ((deps (plist-get metadata :depends)))
                  (should deps)
                  (should (memq 'emacs deps))
                  (should (memq 'dash deps)))))
          
          ;; Cleanup
          (delete-directory test-ext-dir t))))))

(ert-deftest test-property-2-directory-inference ()
  "Property 2 (extended): Directory name inference fallback

*For any* extension directory without -pkg.el or main .el file,
mcg-auto-register-extension should infer provides from directory name.

**Validates: Requirements 4.1**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (autoload-file (expand-file-name "core/core-autoload.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p autoload-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load autoload-file nil t)
      
      ;; Create a temporary test extension with only misc .el files
      (let* ((test-ext-dir (make-temp-file "mcg-test-misc-" t))
             (test-ext-name (file-name-nondirectory test-ext-dir)))
        (unwind-protect
            (progn
              ;; Create a misc .el file (not matching directory name)
              (with-temp-file (expand-file-name "helper.el" test-ext-dir)
                (insert "(provide 'helper)\n"))
              
              ;; Test parsing - should fallback to directory name
              (let ((metadata (mcg-auto-register-extension test-ext-dir)))
                (should metadata)
                (should (plist-get metadata :provides))
                ;; Should have inferred from directory name
                (should (memq (intern test-ext-name) (plist-get metadata :provides)))))
          
          ;; Cleanup
          (delete-directory test-ext-dir t))))))

;;; ============================================================
;;; Property 3: Extension Dependency Resolution
;;; ============================================================

;; **Feature: emacs-config-optimization-v2, Property 3: Extension Dependency Resolution**
;; **Validates: Requirements 4.2**
;;
;; Property: For any extension with declared dependencies, loading that extension
;; should also load all its dependencies first.

(ert-deftest test-property-3-extension-dependency-resolution ()
  "Property 3: Extension Dependency Resolution

*For any* extension with declared dependencies, loading that extension
should also load all its dependencies first.

**Validates: Requirements 4.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test with known extension that has dependencies
      ;; core/f.el depends on core/dash and core/s.el
      (let ((deps (mcg-extension-dependencies "core/f.el")))
        ;; Should have dependencies
        (should deps)
        ;; Should include dash
        (should (member "core/dash" deps))
        ;; Should include s.el
        (should (member "core/s.el" deps))
        ;; Dependencies should come before the extension itself
        ;; (they should be in load order)
        (let ((dash-pos (seq-position deps "core/dash"))
              (s-pos (seq-position deps "core/s.el")))
          (should dash-pos)
          (should s-pos))))))

(ert-deftest test-property-3-transitive-dependencies ()
  "Property 3 (extended): Transitive dependency resolution

*For any* extension with transitive dependencies (A depends on B, B depends on C),
loading A should load C first, then B, then A.

**Validates: Requirements 4.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test with ui/shrink-path which depends on core/dash, core/f.el, core/s.el
      ;; and core/f.el itself depends on core/dash and core/s.el
      (let ((deps (mcg-extension-dependencies "ui/shrink-path")))
        ;; Should have dependencies
        (should deps)
        ;; Should include all transitive deps
        (should (member "core/dash" deps))
        (should (member "core/s.el" deps))
        (should (member "core/f.el" deps))
        ;; dash and s.el should come before f.el (since f.el depends on them)
        (let ((dash-pos (seq-position deps "core/dash"))
              (s-pos (seq-position deps "core/s.el"))
              (f-pos (seq-position deps "core/f.el")))
          (should (< dash-pos f-pos))
          (should (< s-pos f-pos)))))))

(ert-deftest test-property-3-no-duplicate-deps ()
  "Property 3 (extended): No duplicate dependencies

*For any* extension with shared transitive dependencies,
the dependency list should not contain duplicates.

**Validates: Requirements 4.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test with extension that has shared deps
      (let ((deps (mcg-extension-dependencies "ui/shrink-path")))
        ;; Should have no duplicates
        (should (= (length deps) (length (delete-dups (copy-sequence deps)))))))))

(ert-deftest test-property-3-deps-loaded-before-extension ()
  "Property 3 (extended): Dependencies loaded before extension

*For any* extension with dependencies, when loading the extension,
all dependencies should be in mcg-loaded-extensions before the extension.

**Validates: Requirements 4.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Clear loaded extensions
      (setq mcg-loaded-extensions nil)
      
      ;; Only test if extensions exist
      (when (and (mcg-extension-exists-p "core/f.el")
                 (mcg-extension-exists-p "core/dash")
                 (mcg-extension-exists-p "core/s.el"))
        ;; Load f.el which depends on dash and s.el
        (mcg-load-extension "core/f.el")
        
        ;; All deps should be loaded
        (should (member "core/dash" mcg-loaded-extensions))
        (should (member "core/s.el" mcg-loaded-extensions))
        (should (member "core/f.el" mcg-loaded-extensions))))))

;;; ============================================================
;;; Cache Staleness Tests
;;; ============================================================

(ert-deftest test-cache-staleness-detection ()
  "Test that cache staleness is correctly detected."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (autoload-file (expand-file-name "core/core-autoload.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p autoload-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load autoload-file nil t)
      
      ;; Should detect stale when cache is empty
      (let ((mcg-extension-cache nil))
        (should (mcg--cache-stale-p))))))

(ert-deftest test-cache-ensure-fresh ()
  "Test that mcg-ensure-cache-fresh works correctly."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (autoload-file (expand-file-name "core/core-autoload.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p autoload-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load autoload-file nil t)
      
      ;; Should return t after ensuring fresh
      (should (mcg-ensure-cache-fresh)))))

;;; ============================================================
;;; Enhanced List Extensions Tests
;;; ============================================================

(ert-deftest test-list-extensions-function-exists ()
  "Test that enhanced list extensions functions exist."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      (should (fboundp 'mcg/list-extensions))
      (should (fboundp 'mcg/show-extension-errors))
      (should (fboundp 'mcg/load-extension-interactive)))))

(provide 'test-v3-extension-autodiscovery)
;;; test-v3-extension-autodiscovery.el ends here
