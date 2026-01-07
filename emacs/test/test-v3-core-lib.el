;;; test-v3-core-lib.el --- Tests for V3 core-lib -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 core-lib module.
;; Tests verify that extension loading works correctly and that
;; missing extensions do not block startup.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config-v3"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config.")

(defvar test-v3-emacs-dir
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for emacs config.")

;;; ============================================================
;;; Property Tests
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 10: 扩展缺失不阻止启动**
;; **Validates: Requirements 6.3, 8.3**
;;
;; Property: For any registered but missing extension,
;; the system must issue a warning but continue startup.

(ert-deftest test-property-10-missing-extension-does-not-block-startup ()
  "Property 10: 扩展缺失不阻止启动

*For any* registered extension whose directory does not exist,
calling mcg-load-extension must:
1. Return nil (indicating failure)
2. Record the error in mcg-extension-load-errors
3. NOT signal an error that would block startup

**Validates: Requirements 6.3, 8.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      ;; Load the modules
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Clear any previous errors
      (setq mcg-extension-load-errors nil)
      (setq mcg-loaded-extensions nil)
      
      ;; Test with a non-existent extension
      (let ((fake-extension "nonexistent/fake-package-xyz"))
        ;; Add fake extension to registry for testing
        (push (list fake-extension :provides '(fake-feature)) mcg-extensions)
        
        ;; Attempt to load the missing extension
        ;; This should NOT signal an error
        (let ((result (mcg-load-extension fake-extension)))
          ;; Should return nil for missing extension
          (should (null result))
          
          ;; Should have recorded the error
          (should (assoc fake-extension mcg-extension-load-errors))
          
          ;; Should NOT have added to loaded extensions
          (should-not (member fake-extension mcg-loaded-extensions)))
        
        ;; Clean up - remove fake extension from registry
        (setq mcg-extensions (assoc-delete-all fake-extension mcg-extensions))))))

(ert-deftest test-property-10-multiple-missing-extensions ()
  "Property 10 (extended): Multiple missing extensions don't block startup

*For any* set of missing extensions, loading them all should:
1. Record all errors
2. Continue processing remaining extensions
3. Not signal any blocking errors

**Validates: Requirements 6.3, 8.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Clear state
      (setq mcg-extension-load-errors nil)
      (setq mcg-loaded-extensions nil)
      
      ;; Create multiple fake extensions
      (let ((fake-extensions '("fake/ext1" "fake/ext2" "fake/ext3")))
        ;; Add to registry
        (dolist (ext fake-extensions)
          (push (list ext :provides (list (intern (format "%s-feature" ext)))) mcg-extensions))
        
        ;; Load all - should not error
        (let ((loaded-count (mcg-load-extensions fake-extensions)))
          ;; None should have loaded
          (should (= loaded-count 0))
          
          ;; All should have errors recorded
          (should (= (length mcg-extension-load-errors) 3)))
        
        ;; Clean up
        (dolist (ext fake-extensions)
          (setq mcg-extensions (assoc-delete-all ext mcg-extensions)))))))

;;; ============================================================
;;; Unit Tests for core-lib
;;; ============================================================

(ert-deftest test-core-lib-provides-feature ()
  "Test that core-lib provides its feature."
  (let ((lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (file-exists-p lib-file)
      ;; First load bootstrap (dependency)
      (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir)))
        (when (file-exists-p bootstrap-file)
          (load bootstrap-file nil t)))
      (load lib-file nil t)
      (should (featurep 'core-lib)))))

(ert-deftest test-core-lib-extensions-registry-defined ()
  "Test that extensions registry is defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (should (boundp 'mcg-extensions))
      (should (listp mcg-extensions))
      (should (> (length mcg-extensions) 0)))))

(ert-deftest test-core-lib-extension-path-function ()
  "Test mcg-extension-path function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Should return nil for nil input
      (should (null (mcg-extension-path nil)))
      ;; Should return a path for valid input
      (let ((path (mcg-extension-path "core/dash")))
        (should path)
        (should (stringp path))
        (should (string-match-p "core/dash$" path))))))

(ert-deftest test-core-lib-extension-exists-p-function ()
  "Test mcg-extension-exists-p function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; core/dash should exist (it's a common extension)
      (should (mcg-extension-exists-p "core/dash"))
      ;; nonexistent extension should not exist
      (should-not (mcg-extension-exists-p "nonexistent/fake")))))

(ert-deftest test-core-lib-extension-info-function ()
  "Test mcg-extension-info function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Should return info for registered extension
      (let ((info (mcg-extension-info "core/dash")))
        (should info)
        (should (plist-get info :name))
        (should (plist-get info :path))
        (should (plist-get info :provides)))
      ;; Should return nil for unregistered extension
      (should (null (mcg-extension-info "unregistered/package"))))))

(ert-deftest test-core-lib-extension-dependencies-function ()
  "Test mcg-extension-dependencies function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; core/dash has no dependencies
      (should (null (mcg-extension-dependencies "core/dash")))
      ;; core/f.el depends on dash and s.el
      (let ((deps (mcg-extension-dependencies "core/f.el")))
        (should deps)
        (should (member "core/dash" deps))
        (should (member "core/s.el" deps))))))

(ert-deftest test-core-lib-load-extension-existing ()
  "Test mcg-load-extension with existing extension."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Clear state
      (setq mcg-loaded-extensions nil)
      ;; Load an existing extension
      (when (mcg-extension-exists-p "core/dash")
        (let ((result (mcg-load-extension "core/dash")))
          (should result)
          (should (member "core/dash" mcg-loaded-extensions)))))))

(ert-deftest test-core-lib-list-all-extensions ()
  "Test mcg-list-all-extensions function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (let ((all-exts (mcg-list-all-extensions)))
        (should all-exts)
        (should (listp all-exts))
        (should (member "core/dash" all-exts))))))

(ert-deftest test-core-lib-find-extension-by-feature ()
  "Test mcg-find-extension-by-feature function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Should find extension by feature
      (should (equal "core/dash" (mcg-find-extension-by-feature 'dash)))
      (should (equal "completion/vertico" (mcg-find-extension-by-feature 'vertico)))
      ;; Should return nil for unknown feature
      (should (null (mcg-find-extension-by-feature 'nonexistent-feature-xyz))))))

(ert-deftest test-core-lib-doctor-function-exists ()
  "Test that mcg-doctor function exists."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (should (fboundp 'mcg-doctor))
      (should (fboundp 'mcg-verify-core-directories))
      (should (fboundp 'mcg-verify-extensions))
      (should (fboundp 'mcg-verify-module-deps)))))

(ert-deftest test-core-lib-verify-core-directories ()
  "Test mcg-verify-core-directories function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Should return a list (possibly empty if all dirs exist)
      (let ((issues (mcg-verify-core-directories)))
        (should (listp issues))))))

;;; ============================================================
;;; Property 11: 依赖验证完整性
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 11: 依赖验证完整性**
;; **Validates: Requirements 8.2**
;;
;; Property: For any mcg-doctor call, it must check all enabled
;; modules' dependencies are satisfied.

(ert-deftest test-property-11-dependency-verification-completeness ()
  "Property 11: 依赖验证完整性

*For any* mcg-doctor call, the system must check all enabled
modules' dependencies are satisfied.

**Validates: Requirements 8.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test mcg-verify-module-deps with a module that has dependencies
      (let* ((test-module "test-module")
             (deps '((:lang . lsp) (:editor . defaults)))
             (issues (mcg-verify-module-deps test-module deps)))
        ;; Since these modules don't exist yet, we should get issues
        (should (listp issues))
        ;; Each missing dependency should be reported
        (should (>= (length issues) 2))
        ;; Each issue should have the required fields
        (dolist (issue issues)
          (should (plist-get issue :type))
          (should (plist-get issue :message))
          (should (plist-get issue :fix)))))))

(ert-deftest test-property-11-extension-dependency-check ()
  "Property 11 (extended): Extension dependencies are checked

*For any* extension with declared dependencies, mcg-doctor-check-all-deps
must verify all dependencies exist.

**Validates: Requirements 8.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Add a test extension with a missing dependency
      (let ((test-ext "test/ext-with-missing-dep"))
        (push (list test-ext :provides '(test-feature) :depends '("nonexistent/dep"))
              mcg-extensions)
        
        ;; Check all deps should find the missing dependency
        (let ((issues (mcg-doctor-check-all-deps)))
          (should (listp issues))
          ;; Should find at least one issue for our test extension
          (should (seq-some
                   (lambda (issue)
                     (string-match-p "nonexistent/dep"
                                     (plist-get issue :message)))
                   issues)))
        
        ;; Clean up
        (setq mcg-extensions (assoc-delete-all test-ext mcg-extensions))))))

(ert-deftest test-property-11-doctor-runs-all-checks ()
  "Property 11 (supporting): mcg-doctor runs all verification checks

*For any* mcg-doctor invocation, it must run all verification functions.

**Validates: Requirements 8.2, 8.4**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Clear previous issues
      (setq mcg-doctor-issues nil)
      
      ;; Run doctor (non-interactively)
      (let ((issues (mcg-doctor)))
        ;; Should return a list
        (should (listp issues))
        ;; mcg-doctor-issues should be set
        (should (eq issues mcg-doctor-issues))))))

(provide 'test-v3-core-lib)
;;; test-v3-core-lib.el ends here
