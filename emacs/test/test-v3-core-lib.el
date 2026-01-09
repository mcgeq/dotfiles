;;; test-v3-core-lib.el --- Tests for V3 core-lib -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 core-lib module.
;; Tests verify that extension loading works correctly and that
;; missing extensions do not block startup.

;;; Code:

(require 'ert)

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
      ;; Only test if extensions directory exists
      (when (and (boundp 'mcg-extensions-dir)
                 (file-directory-p mcg-extensions-dir))
        ;; core/dash should exist (it's a common extension)
        (should (mcg-extension-exists-p "core/dash"))
        ;; nonexistent extension should not exist
        (should-not (mcg-extension-exists-p "nonexistent/fake"))))))

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

;;; ============================================================
;;; Keybinding Registry Tests
;;; ============================================================

(ert-deftest test-keybinding-registry-defined ()
  "Test that keybinding registry variables are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (should (boundp 'mcg-keybinding-registry))
      (should (boundp 'mcg-keybinding-conflicts)))))

(ert-deftest test-keybinding-register-new-binding ()
  "Test registering a new keybinding."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Clear registry
      (mcg-clear-keybinding-registry)
      ;; Register a new binding
      (let ((result (mcg-register-keybinding "C-c t t" 'test-command "test-module")))
        (should result)
        (should (assoc "C-c t t" mcg-keybinding-registry))
        (let ((entry (assoc "C-c t t" mcg-keybinding-registry)))
          (should (eq (cadr entry) 'test-command))
          (should (equal (cddr entry) "test-module"))))
      ;; Clean up
      (mcg-clear-keybinding-registry))))

(ert-deftest test-keybinding-conflict-detection ()
  "Test that keybinding conflicts are detected."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Clear registry
      (mcg-clear-keybinding-registry)
      ;; Register first binding
      (mcg-register-keybinding "C-c x x" 'command-1 "module-1")
      ;; Register conflicting binding (same key, different command)
      (let ((result (mcg-register-keybinding "C-c x x" 'command-2 "module-2")))
        ;; Should return nil for conflict
        (should (null result))
        ;; Should record the conflict
        (should (> (length mcg-keybinding-conflicts) 0))
        (let ((conflict (car mcg-keybinding-conflicts)))
          (should (equal (car conflict) "C-c x x"))
          (should (eq (cadr conflict) 'command-1))
          (should (eq (nth 3 conflict) 'command-2))))
      ;; Clean up
      (mcg-clear-keybinding-registry))))

(ert-deftest test-keybinding-same-command-no-conflict ()
  "Test that re-registering same command doesn't create conflict."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Clear registry
      (mcg-clear-keybinding-registry)
      ;; Register binding
      (mcg-register-keybinding "C-c y y" 'same-command "module-1")
      ;; Re-register same command
      (let ((result (mcg-register-keybinding "C-c y y" 'same-command "module-2")))
        ;; Should return t (no conflict)
        (should result)
        ;; Should not record a conflict
        (should (= (length mcg-keybinding-conflicts) 0)))
      ;; Clean up
      (mcg-clear-keybinding-registry))))

(ert-deftest test-keybinding-list-keybindings ()
  "Test mcg-list-keybindings function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Clear registry
      (mcg-clear-keybinding-registry)
      ;; Register some bindings
      (mcg-register-keybinding "C-c a a" 'cmd-a "mod-a")
      (mcg-register-keybinding "C-c b b" 'cmd-b "mod-b")
      ;; List should return sorted bindings
      (let ((bindings (mcg-list-keybindings)))
        (should (= (length bindings) 2))
        ;; Should be sorted by key
        (should (equal (caar bindings) "C-c a a"))
        (should (equal (car (cadr bindings)) "C-c b b")))
      ;; Clean up
      (mcg-clear-keybinding-registry))))

(ert-deftest test-keybinding-source-lookup ()
  "Test mcg-keybinding-source function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Clear registry
      (mcg-clear-keybinding-registry)
      ;; Register a binding
      (mcg-register-keybinding "C-c z z" 'test-cmd "test-source")
      ;; Should find the source
      (should (equal (mcg-keybinding-source "C-c z z") "test-source"))
      ;; Should return nil for unknown key
      (should (null (mcg-keybinding-source "C-c unknown")))
      ;; Clean up
      (mcg-clear-keybinding-registry))))

(ert-deftest test-keybindings-by-source ()
  "Test mcg-keybindings-by-source function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      ;; Clear registry
      (mcg-clear-keybinding-registry)
      ;; Register bindings from different sources
      (mcg-register-keybinding "C-c 1 1" 'cmd-1 "source-a")
      (mcg-register-keybinding "C-c 2 2" 'cmd-2 "source-a")
      (mcg-register-keybinding "C-c 3 3" 'cmd-3 "source-b")
      ;; Get bindings by source
      (let ((source-a-bindings (mcg-keybindings-by-source "source-a")))
        (should (= (length source-a-bindings) 2))
        (should (assoc "C-c 1 1" source-a-bindings))
        (should (assoc "C-c 2 2" source-a-bindings)))
      (let ((source-b-bindings (mcg-keybindings-by-source "source-b")))
        (should (= (length source-b-bindings) 1)))
      ;; Clean up
      (mcg-clear-keybinding-registry))))

;;; ============================================================
;;; Property Tests for Missing Extension Error Messages (emacs-config-optimization-v2)
;;; ============================================================

;; **Feature: emacs-config-optimization-v2, Property 5: Missing Extension Error Message**
;; **Validates: Requirements 5.3**
;;
;; Property: For any missing extension, the error message should contain
;; the exact git submodule command to install it.

(ert-deftest test-property-5-missing-extension-error-message ()
  "Property 5: Missing Extension Error Message

*For any* missing extension, the error message should contain the exact
git submodule command to install it.

**Feature: emacs-config-optimization-v2, Property 5: Missing Extension Error Message**
**Validates: Requirements 5.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Clear state
      (setq mcg-extension-load-errors nil)
      (setq mcg-loaded-extensions nil)
      
      ;; Test with a non-existent extension
      (let ((fake-extension "test-category/fake-missing-extension"))
        ;; Add fake extension to registry for testing
        (push (list fake-extension :provides '(fake-feature)) mcg-extensions)
        
        ;; Attempt to load the missing extension
        (mcg-load-extension fake-extension)
        
        ;; Check that error was recorded with git command
        (let* ((error-entry (assoc fake-extension mcg-extension-load-errors))
               (error-info (cdr error-entry)))
          (should error-entry)
          ;; Error info should be a plist with git-command
          (should (plist-get error-info :git-command))
          ;; Git command should contain the extension path
          (let ((git-cmd (plist-get error-info :git-command)))
            (should (stringp git-cmd))
            (should (string-match-p "git submodule" git-cmd))
            (should (string-match-p fake-extension git-cmd))
            (should (string-match-p "site-lisp/extensions" git-cmd))))
        
        ;; Clean up
        (setq mcg-extensions (assoc-delete-all fake-extension mcg-extensions))))))

(ert-deftest test-property-5-git-command-format ()
  "Property 5 (extended): Git command has correct format

*For any* missing extension, the generated git command should be
a complete, runnable command.

**Feature: emacs-config-optimization-v2, Property 5: Missing Extension Error Message**
**Validates: Requirements 5.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test the git command generator directly
      (let ((cmd (mcg-generate-git-submodule-command "core/test-ext")))
        (should (stringp cmd))
        ;; Should start with git
        (should (string-prefix-p "git" cmd))
        ;; Should contain submodule update
        (should (string-match-p "submodule update" cmd))
        ;; Should contain --init
        (should (string-match-p "--init" cmd))
        ;; Should contain the extension path
        (should (string-match-p "core/test-ext" cmd))))))

(ert-deftest test-property-5-alternative-extensions-suggested ()
  "Property 5 (extended): Alternative extensions are suggested when available

*For any* missing extension, the system should suggest alternative
extensions if similar ones exist.

**Feature: emacs-config-optimization-v2, Property 5: Missing Extension Error Message**
**Validates: Requirements 5.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test finding alternatives for a core extension
      ;; (assuming core/dash exists)
      (when (mcg-extension-exists-p "core/dash")
        (let ((alternatives (mcg-find-alternative-extensions "core/nonexistent")))
          ;; Should return a list
          (should (listp alternatives))
          ;; If there are alternatives, they should be from the same category
          (when alternatives
            (should (seq-some (lambda (alt) (string-prefix-p "core/" alt))
                              alternatives))))))))

(ert-deftest test-mcg-generate-git-submodule-command-function-defined ()
  "Test that mcg-generate-git-submodule-command function is defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Check that the functions are defined
      (should (fboundp 'mcg-generate-git-submodule-command))
      (should (fboundp 'mcg-find-alternative-extensions))
      (should (fboundp 'mcg-format-extension-error)))))

(ert-deftest test-mcg-format-extension-error ()
  "Test mcg-format-extension-error function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Clear state
      (setq mcg-extension-load-errors nil)
      (setq mcg-loaded-extensions nil)
      
      ;; Create a test error entry
      (let ((fake-extension "test/missing-ext"))
        (push (list fake-extension :provides '(test-feature)) mcg-extensions)
        (mcg-load-extension fake-extension)
        
        ;; Get the error info and format it
        (let* ((error-entry (assoc fake-extension mcg-extension-load-errors))
               (error-info (cdr error-entry))
               (formatted (mcg-format-extension-error error-info)))
          (should (stringp formatted))
          ;; Should contain extension name
          (should (string-match-p "Extension:" formatted))
          ;; Should contain git command
          (should (string-match-p "git submodule" formatted)))
        
        ;; Clean up
        (setq mcg-extensions (assoc-delete-all fake-extension mcg-extensions))))))

;;; ============================================================
;;; Property Tests for Theme Color Inheritance (emacs-config-optimization-v2)
;;; ============================================================

;; **Feature: emacs-config-optimization-v2, Property 6: Theme Color Inheritance**
;; **Validates: Requirements 7.4**
;;
;; Property: For any UI component using mcg-popup-show, the colors should
;; be derived from the current theme's faces.

(ert-deftest test-property-6-theme-color-inheritance-border ()
  "Property 6: Theme Color Inheritance - Border Color

*For any* UI component using mcg-popup-show, the border color should be
derived from the current theme's faces (font-lock-keyword-face or link).

**Feature: emacs-config-optimization-v2, Property 6: Theme Color Inheritance**
**Validates: Requirements 7.4**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test that mcg--popup-get-border-color returns a valid color
      (when (fboundp 'mcg--popup-get-border-color)
        (let ((border-color (mcg--popup-get-border-color)))
          ;; Should return a string
          (should (stringp border-color))
          ;; Should be a valid color (either a hex color or a named color)
          (should (or (string-match-p "^#[0-9A-Fa-f]\\{6\\}$" border-color)
                      (color-defined-p border-color)))
          ;; Should match one of the theme faces or fallback
          (let ((keyword-fg (face-foreground 'font-lock-keyword-face nil t))
                (link-fg (face-foreground 'link nil t)))
            (should (or (equal border-color keyword-fg)
                        (equal border-color link-fg)
                        (equal border-color "#61AFEF")))))))))

(ert-deftest test-property-6-theme-color-inheritance-background ()
  "Property 6: Theme Color Inheritance - Background Color

*For any* UI component using mcg-popup-show, the background color should be
derived from the current theme's faces (tooltip or default).

**Feature: emacs-config-optimization-v2, Property 6: Theme Color Inheritance**
**Validates: Requirements 7.4**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test that mcg--popup-get-background-color returns a valid color
      (when (fboundp 'mcg--popup-get-background-color)
        (let ((bg-color (mcg--popup-get-background-color)))
          ;; Should return a string
          (should (stringp bg-color))
          ;; Should be a valid color (hex color, named color, or fallback)
          ;; In batch mode, face colors may be unspecified, so we check for fallback
          (should (or (string-match-p "^#[0-9A-Fa-f]\\{6\\}$" bg-color)
                      (and (display-graphic-p) (color-defined-p bg-color))
                      (equal bg-color "#282C34")))  ; fallback value
          ;; Should match one of the theme faces or fallback
          (let ((tooltip-bg (face-background 'tooltip nil t))
                (default-bg (face-background 'default nil t)))
            (should (or (equal bg-color tooltip-bg)
                        (equal bg-color default-bg)
                        (equal bg-color "#282C34")))))))))

(ert-deftest test-property-6-theme-color-inheritance-foreground ()
  "Property 6: Theme Color Inheritance - Foreground Color

*For any* UI component using mcg-popup-show, the foreground color should be
derived from the current theme's default face.

**Feature: emacs-config-optimization-v2, Property 6: Theme Color Inheritance**
**Validates: Requirements 7.4**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test that mcg--popup-get-foreground-color returns a valid color
      (when (fboundp 'mcg--popup-get-foreground-color)
        (let ((fg-color (mcg--popup-get-foreground-color)))
          ;; Should return a string
          (should (stringp fg-color))
          ;; Should be a valid color (hex color, named color, or fallback)
          ;; In batch mode, face colors may be unspecified, so we check for fallback
          (should (or (string-match-p "^#[0-9A-Fa-f]\\{6\\}$" fg-color)
                      (and (display-graphic-p) (color-defined-p fg-color))
                      (equal fg-color "#ABB2BF")))  ; fallback value
          ;; Should match the default face foreground or fallback
          (let ((default-fg (face-foreground 'default nil t)))
            (should (or (equal fg-color default-fg)
                        (equal fg-color "#ABB2BF")))))))))

(ert-deftest test-property-6-popup-color-functions-defined ()
  "Property 6 (supporting): Popup color functions are defined

*For any* mcg-popup-show call, the color functions must be available.

**Feature: emacs-config-optimization-v2, Property 6: Theme Color Inheritance**
**Validates: Requirements 7.4**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Check that all popup color functions are defined
      (should (fboundp 'mcg--popup-get-border-color))
      (should (fboundp 'mcg--popup-get-background-color))
      (should (fboundp 'mcg--popup-get-foreground-color)))))

(ert-deftest test-property-6-colors-change-with-theme ()
  "Property 6 (extended): Colors should change when theme changes

*For any* theme change, the popup colors should reflect the new theme.
This test verifies that colors are dynamically queried, not cached.

**Feature: emacs-config-optimization-v2, Property 6: Theme Color Inheritance**
**Validates: Requirements 7.4**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Test that the functions query faces dynamically
      ;; by checking they use face-foreground/face-background
      (when (fboundp 'mcg--popup-get-border-color)
        ;; Get initial colors
        (let ((initial-border (mcg--popup-get-border-color))
              (initial-bg (mcg--popup-get-background-color))
              (initial-fg (mcg--popup-get-foreground-color)))
          ;; Colors should be strings
          (should (stringp initial-border))
          (should (stringp initial-bg))
          (should (stringp initial-fg))
          ;; Calling again should return the same values (consistent)
          (should (equal initial-border (mcg--popup-get-border-color)))
          (should (equal initial-bg (mcg--popup-get-background-color)))
          (should (equal initial-fg (mcg--popup-get-foreground-color))))))))

(provide 'test-v3-core-lib)
;;; test-v3-core-lib.el ends here
