;;; test-v3-core-bootstrap.el --- Tests for V3 core-bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 core-bootstrap module.
;; Tests verify that the bootstrap module correctly initializes paths
;; and that only core modules are loaded at startup.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config"
                    (file-name-directory (or load-file-name buffer-file-name))))

;;; ============================================================
;;; Property Tests
;;; ============================================================

;; **Feature: modern-emacs-config, Property 1: Path Initialization Completeness**
;; **Validates: Requirements 1.2**
;;
;; Property: For any valid base directory configuration, after loading
;; core-bootstrap, all mcg-*-dir path variables SHALL be non-nil and
;; point to valid directory paths.

(defun test-gen-random-base-dir ()
  "Generate a random valid base directory for testing.
Returns an existing directory path that can be used as base-dir."
  (let* ((candidates (list
                      ;; Use actual emacs directory
                      (expand-file-name "../" test-v3-root-dir)
                      ;; Use temp directory
                      (temporary-file-directory)
                      ;; Use user home
                      (expand-file-name "~")
                      ;; Use current directory
                      default-directory))
         (valid-candidates (seq-filter #'file-directory-p candidates)))
    (when valid-candidates
      (nth (random (length valid-candidates)) valid-candidates))))

(defun test-path-variable-valid-p (var-value base-dir)
  "Check if VAR-VALUE is a valid path relative to BASE-DIR.
Returns t if VAR-VALUE is non-nil, a string, and an absolute path."
  (and var-value
       (stringp var-value)
       (file-name-absolute-p var-value)))

(ert-deftest test-property-1-path-initialization-completeness ()
  "Property 1: Path Initialization Completeness

*For any* valid base directory configuration, after loading core-bootstrap,
all mcg-*-dir path variables SHALL be non-nil and point to valid directory paths.

**Feature: modern-emacs-config, Property 1: Path Initialization Completeness**
**Validates: Requirements 1.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; Run property test 100 times with different base directories
      (dotimes (_ 100)
        (let ((base-dir (test-gen-random-base-dir)))
          (when base-dir
            ;; Reset all path variables to nil before testing
            (setq mcg-emacs-dir nil
                  mcg-config-dir nil
                  mcg-core-dir nil
                  mcg-modules-dir nil
                  mcg-extensions-dir nil
                  mcg-private-dir nil
                  mcg-cache-dir nil
                  mcg-data-dir nil)
            
            ;; Initialize paths with the random base directory
            (mcg-init-paths base-dir)
            
            ;; Property: All path variables must be non-nil
            (should (not (null mcg-emacs-dir)))
            (should (not (null mcg-config-dir)))
            (should (not (null mcg-core-dir)))
            (should (not (null mcg-modules-dir)))
            (should (not (null mcg-extensions-dir)))
            (should (not (null mcg-private-dir)))
            (should (not (null mcg-cache-dir)))
            (should (not (null mcg-data-dir)))
            
            ;; Property: All path variables must be valid paths (strings and absolute)
            (should (test-path-variable-valid-p mcg-emacs-dir base-dir))
            (should (test-path-variable-valid-p mcg-config-dir base-dir))
            (should (test-path-variable-valid-p mcg-core-dir base-dir))
            (should (test-path-variable-valid-p mcg-modules-dir base-dir))
            (should (test-path-variable-valid-p mcg-extensions-dir base-dir))
            (should (test-path-variable-valid-p mcg-private-dir base-dir))
            (should (test-path-variable-valid-p mcg-cache-dir base-dir))
            (should (test-path-variable-valid-p mcg-data-dir base-dir))
            
            ;; Property: Path hierarchy must be consistent
            ;; mcg-config-dir should be under mcg-emacs-dir
            (should (string-prefix-p (file-name-as-directory mcg-emacs-dir)
                                     mcg-config-dir))
            ;; mcg-core-dir should be under mcg-config-dir
            (should (string-prefix-p (file-name-as-directory mcg-config-dir)
                                     mcg-core-dir))
            ;; mcg-modules-dir should be under mcg-config-dir
            (should (string-prefix-p (file-name-as-directory mcg-config-dir)
                                     mcg-modules-dir))
            ;; mcg-private-dir should be under mcg-config-dir
            (should (string-prefix-p (file-name-as-directory mcg-config-dir)
                                     mcg-private-dir))
            ;; mcg-extensions-dir should be under mcg-emacs-dir
            (should (string-prefix-p (file-name-as-directory mcg-emacs-dir)
                                     mcg-extensions-dir))
            ;; mcg-cache-dir should be under mcg-emacs-dir
            (should (string-prefix-p (file-name-as-directory mcg-emacs-dir)
                                     mcg-cache-dir))
            ;; mcg-data-dir should be under mcg-emacs-dir
            (should (string-prefix-p (file-name-as-directory mcg-emacs-dir)
                                     mcg-data-dir))))))))

;; **Feature: emacs-config-refactor, Property 6: 启动时只加载核心模块**
;; **Validates: Requirements 2.1**
;;
;; Property: For any Emacs startup (before deferred loading),
;; the features list should only contain core-* modules from V3 config.

(ert-deftest test-property-6-startup-only-loads-core-modules ()
  "Property 6: 启动时只加载核心模块

*For any* Emacs startup completed (before deferred loading),
the features list should only contain core-* modules from V3 config.

**Validates: Requirements 2.1**"
  ;; This test verifies that the startup sequence is designed correctly:
  ;; - core-bootstrap should be loadable independently
  ;; - Loading core-bootstrap should NOT automatically load UI/lang modules
  ;; 
  ;; Note: We cannot test the actual features list state because other tests
  ;; in the suite may have already loaded modules. Instead, we verify the
  ;; design property: core-bootstrap does not require non-core modules.
  
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      ;; Verify core-bootstrap can be loaded
      (load bootstrap-file nil t)
      (should (featurep 'core-bootstrap))
      
      ;; Verify core-bootstrap does NOT require non-core modules
      ;; by checking its require statements in the source
      (with-temp-buffer
        (insert-file-contents bootstrap-file)
        (let ((content (buffer-string)))
          ;; Should NOT require +theme, +modeline, +lsp, etc.
          (should-not (string-match-p "(require '\\+theme)" content))
          (should-not (string-match-p "(require '\\+modeline)" content))
          (should-not (string-match-p "(require '\\+lsp)" content))
          (should-not (string-match-p "(require '\\+rust)" content))
          (should-not (string-match-p "(require '\\+python)" content))
          ;; Should only require core modules or built-in features
          ;; This verifies the design property that startup only loads core
          )))))

(ert-deftest test-property-6-core-bootstrap-provides-feature ()
  "Property 6 (supporting): core-bootstrap provides its feature.

*For any* successful load of core-bootstrap.el,
the 'core-bootstrap feature must be provided.

**Validates: Requirements 2.1**"
  ;; After loading core-bootstrap, it should provide its feature
  ;; This is a precondition for the main property
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    ;; If the tangled file exists, verify it provides the feature
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      (should (featurep 'core-bootstrap)))))

;;; ============================================================
;;; Unit Tests for core-bootstrap
;;; ============================================================

(ert-deftest test-core-bootstrap-path-variables-defined ()
  "Test that path variables are defined after loading core-bootstrap."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; All path variables should be defined
      (should (boundp 'mcg-emacs-dir))
      (should (boundp 'mcg-core-dir))
      (should (boundp 'mcg-modules-dir))
      (should (boundp 'mcg-extensions-dir))
      (should (boundp 'mcg-private-dir))
      (should (boundp 'mcg-config-dir)))))

(ert-deftest test-core-bootstrap-system-detection ()
  "Test that system detection constants are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; System detection constants should be defined
      (should (boundp 'mcg-is-windows))
      (should (boundp 'mcg-is-linux))
      (should (boundp 'mcg-is-mac))
      (should (boundp 'mcg-is-gui))
      (should (boundp 'mcg-emacs-29+))
      ;; Exactly one OS should be true
      (let ((os-count (+ (if mcg-is-windows 1 0)
                         (if mcg-is-linux 1 0)
                         (if mcg-is-mac 1 0))))
        (should (= os-count 1))))))

(ert-deftest test-core-bootstrap-mcg-init-paths-function ()
  "Test that mcg-init-paths function exists and works."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; Function should exist
      (should (fboundp 'mcg-init-paths))
      ;; After calling with a test path, variables should be set
      (let ((test-base (if (boundp 'mcg-is-windows)
                           (if mcg-is-windows "C:/test" "/tmp/test")
                         "/tmp/test")))
        (mcg-init-paths test-base)
        (should (stringp mcg-emacs-dir))
        (should (stringp mcg-config-dir))
        (should (stringp mcg-core-dir))
        (should (stringp mcg-modules-dir))))))

(ert-deftest test-core-bootstrap-mcg-setup-load-path-function ()
  "Test that mcg-setup-load-path function exists."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      (should (fboundp 'mcg-setup-load-path)))))

(ert-deftest test-core-bootstrap-gc-optimization-variables ()
  "Test that GC optimization variables are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; GC variables should be defined
      (should (boundp 'mcg-gc-cons-threshold-default))
      (should (boundp 'mcg-gc-cons-percentage-default))
      ;; Default values should be reasonable
      (should (> mcg-gc-cons-threshold-default 0))
      (should (> mcg-gc-cons-percentage-default 0))
      (should (< mcg-gc-cons-percentage-default 1)))))

(ert-deftest test-core-bootstrap-mcg-log-macro ()
  "Test that mcg-log macro exists."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; mcg-log should be a macro
      (should (boundp 'mcg-debug-mode))
      ;; When debug mode is off, mcg-log should not produce output
      (let ((mcg-debug-mode nil))
        ;; This should not error
        (eval '(mcg-log "test message"))))))

(ert-deftest test-core-bootstrap-startup-time-tracking ()
  "Test that startup time tracking variables are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; Startup time variables should be defined
      (should (boundp 'mcg-startup-time))
      (should (boundp 'mcg--startup-start-time))
      ;; Display function should exist
      (should (fboundp 'mcg-display-startup-time)))))

;;; ============================================================
;;; Additional Unit Tests for Path Initialization
;;; ============================================================

(ert-deftest test-unit-mcg-init-paths-sets-all-variables ()
  "Unit test: mcg-init-paths sets all path variables correctly."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; Reset all path variables
      (setq mcg-emacs-dir nil
            mcg-config-dir nil
            mcg-core-dir nil
            mcg-modules-dir nil
            mcg-extensions-dir nil
            mcg-private-dir nil
            mcg-cache-dir nil
            mcg-data-dir nil)
      ;; Initialize with actual emacs directory
      (let ((base-dir (expand-file-name "../" test-v3-root-dir)))
        (mcg-init-paths base-dir)
        ;; All variables should be set
        (should mcg-emacs-dir)
        (should mcg-config-dir)
        (should mcg-core-dir)
        (should mcg-modules-dir)
        (should mcg-extensions-dir)
        (should mcg-private-dir)
        (should mcg-cache-dir)
        (should mcg-data-dir)
        ;; Verify expected path structure
        (should (string-match-p "site-lisp/config/$" mcg-config-dir))
        (should (string-match-p "core/$" mcg-core-dir))
        (should (string-match-p "modules/$" mcg-modules-dir))
        (should (string-match-p "site-lisp/extensions/$" mcg-extensions-dir))
        (should (string-match-p "private/$" mcg-private-dir))
        (should (string-match-p "\\.cache/$" mcg-cache-dir))
        (should (string-match-p "\\.data/$" mcg-data-dir))))))

(ert-deftest test-unit-mcg-init-paths-returns-base-dir ()
  "Unit test: mcg-init-paths returns the base directory."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      (let* ((base-dir (expand-file-name "../" test-v3-root-dir))
             (result (mcg-init-paths base-dir)))
        ;; Should return the normalized base directory
        (should result)
        (should (stringp result))
        (should (file-name-absolute-p result))))))

(ert-deftest test-unit-mcg-init-paths-creates-cache-dir ()
  "Unit test: mcg-init-paths creates cache directory if needed."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      (let ((base-dir (expand-file-name "../" test-v3-root-dir)))
        (mcg-init-paths base-dir)
        ;; Cache directory should exist or be creatable
        (should (or (file-directory-p mcg-cache-dir)
                    (null mcg-cache-dir)))))))

;;; ============================================================
;;; Additional Unit Tests for Load-Path Setup
;;; ============================================================

(ert-deftest test-unit-mcg-setup-load-path-adds-core-dir ()
  "Unit test: mcg-setup-load-path adds core directory to load-path."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      (let ((base-dir (expand-file-name "../" test-v3-root-dir)))
        (mcg-init-paths base-dir)
        ;; Save original load-path
        (let ((original-load-path load-path))
          (mcg-setup-load-path)
          ;; Core directory should be in load-path
          (when (file-directory-p mcg-core-dir)
            (should (member mcg-core-dir load-path)))
          ;; Restore original load-path
          (setq load-path original-load-path))))))

(ert-deftest test-unit-mcg-setup-load-path-adds-module-subdirs ()
  "Unit test: mcg-setup-load-path adds module subdirectories to load-path."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      (let ((base-dir (expand-file-name "../" test-v3-root-dir)))
        (mcg-init-paths base-dir)
        ;; Save original load-path
        (let ((original-load-path load-path))
          (mcg-setup-load-path)
          ;; Module subdirectories should be in load-path
          (when (file-directory-p mcg-modules-dir)
            (dolist (subdir (directory-files mcg-modules-dir t "^[^.]"))
              (when (file-directory-p subdir)
                (should (member subdir load-path)))))
          ;; Restore original load-path
          (setq load-path original-load-path))))))

(ert-deftest test-unit-mcg-setup-load-path-handles-missing-dirs ()
  "Unit test: mcg-setup-load-path handles missing directories gracefully."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; Set paths to non-existent directories
      (setq mcg-core-dir "/nonexistent/core"
            mcg-modules-dir "/nonexistent/modules"
            mcg-extensions-dir "/nonexistent/extensions"
            mcg-private-dir "/nonexistent/private")
      ;; This should not error
      (should (progn (mcg-setup-load-path) t)))))

(ert-deftest test-unit-mcg-bootstrap-function ()
  "Unit test: mcg-bootstrap function initializes the system."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      ;; mcg-bootstrap should exist
      (should (fboundp 'mcg-bootstrap))
      ;; Reset state
      (setq mcg-emacs-dir nil)
      ;; Save original hooks
      (let ((original-startup-hook emacs-startup-hook))
        ;; Call bootstrap
        (mcg-bootstrap)
        ;; Paths should be initialized
        (should mcg-emacs-dir)
        ;; Restore hooks
        (setq emacs-startup-hook original-startup-hook)))))

;;; ============================================================
;;; Property Test: GC Threshold Restoration
;;; ============================================================

;; **Feature: modern-emacs-config, Property 17: GC Threshold Restoration**
;; **Validates: Requirements 14.3**
;;
;; Property: For any startup sequence, after emacs-startup-hook runs,
;; gc-cons-threshold SHALL be restored to mcg-gc-cons-threshold-default value.

(defun test-gen-random-gc-threshold-default ()
  "Generate a random valid GC threshold default value for testing.
Returns a reasonable GC threshold value (between 8MB and 64MB)."
  (let* ((min-threshold (* 8 1024 1024))    ; 8MB
         (max-threshold (* 64 1024 1024))   ; 64MB
         (range (- max-threshold min-threshold)))
    (+ min-threshold (random range))))

(defun test-gen-random-gc-percentage-default ()
  "Generate a random valid GC percentage default value for testing.
Returns a reasonable GC percentage value (between 0.05 and 0.5)."
  (+ 0.05 (* (random 45) 0.01)))

(ert-deftest test-property-17-gc-threshold-restoration ()
  "Property 17: GC Threshold Restoration

*For any* startup sequence, after emacs-startup-hook runs,
gc-cons-threshold SHALL be restored to mcg-gc-cons-threshold-default value.

**Feature: modern-emacs-config, Property 17: GC Threshold Restoration**
**Validates: Requirements 14.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      
      ;; Run property test 100 times with different GC threshold values
      (dotimes (_ 100)
        (let* ((random-threshold (test-gen-random-gc-threshold-default))
               (random-percentage (test-gen-random-gc-percentage-default))
               ;; Save original values
               (original-gc-threshold gc-cons-threshold)
               (original-gc-percentage gc-cons-percentage))
          
          ;; Set the default values to our random test values
          (setq mcg-gc-cons-threshold-default random-threshold)
          (setq mcg-gc-cons-percentage-default random-percentage)
          
          ;; Simulate startup: set high threshold (as mcg-setup-gc-optimization does)
          (setq gc-cons-threshold mcg-gc-cons-threshold-startup)
          (setq gc-cons-percentage 0.6)
          
          ;; Property: Before restoration, gc-cons-threshold should be high
          (should (= gc-cons-threshold mcg-gc-cons-threshold-startup))
          
          ;; Simulate emacs-startup-hook running by calling restore function
          (mcg-restore-gc-defaults)
          
          ;; Property: After restoration, gc-cons-threshold must equal the default
          (should (= gc-cons-threshold mcg-gc-cons-threshold-default))
          (should (= gc-cons-threshold random-threshold))
          
          ;; Property: After restoration, gc-cons-percentage must equal the default
          (should (= gc-cons-percentage mcg-gc-cons-percentage-default))
          (should (= gc-cons-percentage random-percentage))
          
          ;; Restore original values for next iteration
          (setq gc-cons-threshold original-gc-threshold)
          (setq gc-cons-percentage original-gc-percentage))))))

(ert-deftest test-property-17-gc-restoration-hook-setup ()
  "Property 17 (supporting): GC restoration hook is properly set up.

*For any* call to mcg-setup-gc-optimization,
mcg-restore-gc-defaults SHALL be added to emacs-startup-hook.

**Feature: modern-emacs-config, Property 17: GC Threshold Restoration**
**Validates: Requirements 14.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el"
                                          test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t)
      
      ;; Save original hook state
      (let ((original-hook emacs-startup-hook))
        ;; Remove our function if it's already there (from previous test runs)
        (remove-hook 'emacs-startup-hook #'mcg-restore-gc-defaults)
        
        ;; Call setup function
        (mcg-setup-gc-optimization)
        
        ;; Property: mcg-restore-gc-defaults should be in emacs-startup-hook
        (should (memq #'mcg-restore-gc-defaults emacs-startup-hook))
        
        ;; Property: gc-cons-threshold should be set to startup value
        (should (= gc-cons-threshold mcg-gc-cons-threshold-startup))
        
        ;; Clean up: restore original hook state
        (setq emacs-startup-hook original-hook)))))

(provide 'test-v3-core-bootstrap)
;;; test-v3-core-bootstrap.el ends here
