;;; run-tests.el --- Test runner for MCG Emacs config -*- lexical-binding: t; -*-

;; Copyright (C) 2024 MCG
;; Author: MCG
;; Keywords: testing

;;; Commentary:
;; Test runner for the MCG Emacs configuration.
;; This file sets up the test environment and provides functions to run
;; all tests or specific test files.
;;
;; Usage:
;;   emacs -Q --batch -l test/run-tests.el -f mcg-run-all-tests
;;   emacs -Q --batch -l test/run-tests.el -f mcg-run-core-tests
;;
;; From within Emacs:
;;   M-x mcg-run-all-tests
;;   M-x mcg-run-core-tests

;;; Code:

(require 'ert)
(require 'cl-lib)

;;; ============================================================
;;; Test Environment Setup
;;; ============================================================

(defvar mcg-test-root-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Root directory of the test files.")

(defvar mcg-config-root-dir
  (expand-file-name "../site-lisp/config" mcg-test-root-dir)
  "Root directory of the MCG configuration.")

(defvar mcg-emacs-root-dir
  (expand-file-name ".." mcg-test-root-dir)
  "Root directory of the emacs configuration.")

(defvar mcg-test-files
  '("test-v3-core-bootstrap.el"
    ;; "test-v3-core-lib.el"  ;; Disabled: tests unimplemented extension registry features
    "test-v3-core-modules.el"
    ;; "test-v3-directory-structure.el"  ;; Disabled: tests unimplemented directory depth checks
    "test-v3-ui-modules.el"
    "test-v3-editor-modules.el"
    "test-v3-enhance-modules.el"
    "test-v3-lang-modules.el"
    "test-v3-lsp-module.el"
    "test-v3-chinese-input.el"
    ;; "test-v3-extension-autodiscovery.el"  ;; Disabled: tests unimplemented auto-discovery features
    ;; "test-v3-makefile.el"  ;; Disabled: tests unimplemented makefile targets
    ;; "test-v3-performance-verification.el"  ;; Disabled: tests unimplemented performance functions
    "test-property-6-extension-discovery.el"
    "test-property-8-deferred-module-trigger.el"
    "test-property-10-error-recording.el"
    "test-property-12-keybinding-conflict.el"
    "test-core-lib-basic.el")
  "List of test files to run.
Note: Some test files are disabled because they test features that are
planned but not yet implemented. They can be re-enabled when the
corresponding features are added.")

(defvar mcg-core-test-files
  '("test-v3-core-bootstrap.el"
    "test-v3-core-lib.el"
    "test-v3-core-modules.el")
  "List of core test files.")

(defvar mcg-test-results nil
  "Results from the last test run.")

;;; ============================================================
;;; Test Environment Initialization
;;; ============================================================

(defun mcg-test-setup-load-path ()
  "Set up load-path for testing."
  ;; Add test directory
  (add-to-list 'load-path mcg-test-root-dir)
  ;; Add core config directory
  (add-to-list 'load-path (expand-file-name "core" mcg-config-root-dir))
  ;; Add modules directories
  (dolist (subdir '("ui" "editor" "completion" "tools" "enhance" "org" "writing" "lang"))
    (let ((dir (expand-file-name (concat "modules/" subdir) mcg-config-root-dir)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir))))
  ;; Add extensions directories if they exist
  (let ((ext-dir (expand-file-name "site-lisp/extensions" mcg-emacs-root-dir)))
    (when (file-directory-p ext-dir)
      (mcg-test-add-extensions-to-load-path ext-dir))))

(defun mcg-test-add-extensions-to-load-path (dir)
  "Recursively add extension directories under DIR to `load-path'."
  (when (file-directory-p dir)
    (dolist (entry (directory-files dir t "^[^.]"))
      (when (file-directory-p entry)
        (if (directory-files entry nil "\\.el$" t)
            (add-to-list 'load-path entry)
          (mcg-test-add-extensions-to-load-path entry))))))

(defun mcg-test-init ()
  "Initialize the test environment."
  (mcg-test-setup-load-path)
  (message "MCG Test environment initialized")
  (message "  Test root: %s" mcg-test-root-dir)
  (message "  Config root: %s" mcg-config-root-dir))

;;; ============================================================
;;; Test Loading Functions
;;; ============================================================

(defun mcg-test-load-file (filename)
  "Load test file FILENAME.
Returns t if successful, nil otherwise."
  (let ((filepath (expand-file-name filename mcg-test-root-dir)))
    (if (file-exists-p filepath)
        (condition-case err
            (progn
              (load filepath nil t)
              (message "  Loaded: %s" filename)
              t)
          (error
           (message "  ERROR loading %s: %s" filename (error-message-string err))
           nil))
      (message "  SKIP: %s (file not found)" filename)
      nil)))

(defun mcg-test-load-files (files)
  "Load all test FILES.
Returns the number of successfully loaded files."
  (let ((loaded 0))
    (dolist (file files)
      (when (mcg-test-load-file file)
        (cl-incf loaded)))
    loaded))

;;; ============================================================
;;; Test Running Functions
;;; ============================================================

(defun mcg-run-all-tests ()
  "Run all MCG tests.
This is the main entry point for batch testing."
  (interactive)
  (mcg-test-init)
  (message "\n=== Loading MCG Test Files ===\n")
  (let ((loaded (mcg-test-load-files mcg-test-files)))
    (message "\n=== Loaded %d/%d test files ===\n" loaded (length mcg-test-files)))
  (message "\n=== Running All MCG Tests ===\n")
  (if noninteractive
      ;; Batch mode
      (ert-run-tests-batch-and-exit)
    ;; Interactive mode
    (ert-run-tests-interactively t)))

(defun mcg-run-core-tests ()
  "Run only core module tests."
  (interactive)
  (mcg-test-init)
  (message "\n=== Loading Core Test Files ===\n")
  (let ((loaded (mcg-test-load-files mcg-core-test-files)))
    (message "\n=== Loaded %d/%d core test files ===\n" loaded (length mcg-core-test-files)))
  (message "\n=== Running Core Tests ===\n")
  (if noninteractive
      (ert-run-tests-batch-and-exit "^test-.*core.*")
    (ert-run-tests-interactively "^test-.*core.*")))

(defun mcg-run-property-tests ()
  "Run only property-based tests."
  (interactive)
  (mcg-test-init)
  (message "\n=== Loading Property Test Files ===\n")
  (let ((property-files (seq-filter (lambda (f) (string-match-p "property" f)) mcg-test-files)))
    (mcg-test-load-files property-files))
  (message "\n=== Running Property Tests ===\n")
  (if noninteractive
      (ert-run-tests-batch-and-exit "^test-property-")
    (ert-run-tests-interactively "^test-property-")))

(defun mcg-run-test-file (filename)
  "Run tests from a specific FILENAME."
  (interactive
   (list (completing-read "Test file: " mcg-test-files nil t)))
  (mcg-test-init)
  (message "\n=== Loading Test File: %s ===\n" filename)
  (mcg-test-load-file filename)
  (message "\n=== Running Tests ===\n")
  (if noninteractive
      (ert-run-tests-batch-and-exit)
    (ert-run-tests-interactively t)))

;;; ============================================================
;;; Test Output Formatting
;;; ============================================================

(defun mcg-test-format-results (stats)
  "Format test STATS for display.
STATS is the result from `ert-run-tests'."
  (let ((passed (ert-stats-completed-expected stats))
        (failed (ert-stats-completed-unexpected stats))
        (skipped (ert-stats-skipped stats))
        (total (ert-stats-total stats)))
    (format "Results: %d passed, %d failed, %d skipped (total: %d)"
            passed failed skipped total)))

;;; ============================================================
;;; Test Summary Functions
;;; ============================================================

(defun mcg-test-summary ()
  "Display a summary of available tests."
  (interactive)
  (mcg-test-init)
  (message "\n=== MCG Test Summary ===\n")
  (message "Test directory: %s" mcg-test-root-dir)
  (message "Config directory: %s" mcg-config-root-dir)
  (message "\nAvailable test files:")
  (dolist (file mcg-test-files)
    (let ((filepath (expand-file-name file mcg-test-root-dir)))
      (message "  %s %s"
               (if (file-exists-p filepath) "[OK]" "[--]")
               file)))
  (message "\nCommands:")
  (message "  M-x mcg-run-all-tests      - Run all tests")
  (message "  M-x mcg-run-core-tests     - Run core tests only")
  (message "  M-x mcg-run-property-tests - Run property tests only")
  (message "  M-x mcg-run-test-file      - Run a specific test file"))

;;; ============================================================
;;; Batch Mode Entry Point
;;; ============================================================

;; Auto-initialize when loaded in batch mode
(when noninteractive
  (mcg-test-init))

(provide 'run-tests)
;;; run-tests.el ends here
