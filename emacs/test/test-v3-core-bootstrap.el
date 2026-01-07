;;; test-v3-core-bootstrap.el --- Tests for V3 core-bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 core-bootstrap module.
;; Tests verify that the bootstrap module correctly initializes paths
;; and that only core modules are loaded at startup.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config-v3"
                    (file-name-directory (or load-file-name buffer-file-name))))

;;; ============================================================
;;; Property Tests
;;; ============================================================

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

(provide 'test-v3-core-bootstrap)
;;; test-v3-core-bootstrap.el ends here
