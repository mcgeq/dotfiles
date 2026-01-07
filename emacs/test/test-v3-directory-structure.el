;;; test-v3-directory-structure.el --- Tests for V3 directory structure -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 directory structure.
;; Tests verify that the directory structure follows the depth limit
;; specified in the requirements.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../config-org-v3"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config.")

(defvar test-v3-modules-dir
  (expand-file-name "modules" test-v3-root-dir)
  "Modules directory for V3 config.")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun test-v3--get-relative-depth (file base-dir)
  "Get the relative depth of FILE from BASE-DIR.
Returns the number of path components in the relative path.
For example, 'category/+module.org' has depth 2 from base-dir."
  (let* ((relative-path (file-relative-name file base-dir))
         (components (split-string relative-path "[/\\\\]" t)))
    (length components)))

(defun test-v3--collect-org-files (dir)
  "Recursively collect all .org files under DIR."
  (let ((files '()))
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively dir "\\.org$"))
        (push file files)))
    (nreverse files)))

(defun test-v3--collect-module-files (dir)
  "Recursively collect all module files (.org and .el) under DIR.
Excludes .gitkeep files. Module files start with + prefix."
  (let ((files '()))
    (when (file-directory-p dir)
      ;; Collect .org files
      (dolist (file (directory-files-recursively dir "\\.org$"))
        (when (string-prefix-p "+" (file-name-nondirectory file))
          (push file files)))
      ;; Collect .el files
      (dolist (file (directory-files-recursively dir "\\.el$"))
        (when (string-prefix-p "+" (file-name-nondirectory file))
          (push file files))))
    (nreverse files)))

(defun test-v3--is-module-file-p (file)
  "Check if FILE is a module file (starts with + prefix)."
  (let ((basename (file-name-nondirectory file)))
    (string-prefix-p "+" basename)))

;;; ============================================================
;;; Property 8: 目录结构深度限制
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 8: 目录结构深度限制**
;; **Validates: Requirements 4.1, 4.2**
;;
;; Property: For any configuration file under modules/, its relative path
;; depth must not exceed 2 levels (modules/category/+module.org).

(ert-deftest test-property-8-directory-structure-depth-limit ()
  "Property 8: 目录结构深度限制

*For any* modules/ 下的配置文件，其相对路径深度不超过 2 级
（modules/category/+module.org）

**Validates: Requirements 4.1, 4.2**"
  (skip-unless (file-directory-p test-v3-modules-dir))
  
  (let ((module-files (test-v3--collect-module-files test-v3-modules-dir))
        (violations '()))
    ;; Check each module file
    (dolist (file module-files)
      (let ((depth (test-v3--get-relative-depth file test-v3-modules-dir)))
        ;; Depth should be exactly 2: category/+module.org
        ;; depth = 2 means: category (1) + file (counted as 1 in path components - 1)
        (when (> depth 2)
          (push (cons file depth) violations))))
    
    ;; Report violations if any
    (when violations
      (message "Directory structure violations found:")
      (dolist (v violations)
        (message "  %s (depth: %d)" (car v) (cdr v))))
    
    ;; Assert no violations
    (should (null violations))))

(ert-deftest test-property-8-all-module-files-at-correct-depth ()
  "Property 8 (extended): All module files are at depth 2

*For any* module file (+*.org or +*.el) under modules/,
it must be at exactly depth 2 (modules/category/+module.ext).

**Validates: Requirements 4.1, 4.2**"
  (skip-unless (file-directory-p test-v3-modules-dir))
  
  (let ((module-files (test-v3--collect-module-files test-v3-modules-dir))
        (correct-depth-files '())
        (incorrect-depth-files '()))
    
    (dolist (file module-files)
      (let ((depth (test-v3--get-relative-depth file test-v3-modules-dir)))
        (if (= depth 2)
            (push file correct-depth-files)
          (push (cons file depth) incorrect-depth-files))))
    
    ;; All module files should be at depth 2
    (should (> (length correct-depth-files) 0))
    (should (null incorrect-depth-files))))

(ert-deftest test-property-8-no-nested-subdirectories-in-categories ()
  "Property 8 (extended): No nested subdirectories in category folders

*For any* category folder under modules/, it must not contain
subdirectories (only module files).

**Validates: Requirements 4.1, 4.2**"
  (skip-unless (file-directory-p test-v3-modules-dir))
  
  (let ((categories (directory-files test-v3-modules-dir t "^[^.]"))
        (violations '()))
    
    (dolist (category-dir categories)
      (when (file-directory-p category-dir)
        ;; Check for subdirectories within the category
        (dolist (item (directory-files category-dir t "^[^.]"))
          (when (and (file-directory-p item)
                     (not (string-match-p "\\.git" item)))
            (push (file-relative-name item test-v3-modules-dir) violations)))))
    
    ;; Report violations if any
    (when violations
      (message "Nested subdirectories found in categories:")
      (dolist (v violations)
        (message "  %s" v)))
    
    ;; Assert no nested subdirectories
    (should (null violations))))

;;; ============================================================
;;; Additional Structure Validation Tests
;;; ============================================================

(ert-deftest test-modules-directory-exists ()
  "Test that the modules directory exists."
  (should (file-directory-p test-v3-modules-dir)))

(ert-deftest test-expected-categories-exist ()
  "Test that expected category directories exist."
  (skip-unless (file-directory-p test-v3-modules-dir))
  
  (let ((expected-categories '("ui" "editor" "completion" "tools" "org" "lang")))
    (dolist (category expected-categories)
      (let ((category-dir (expand-file-name category test-v3-modules-dir)))
        (should (file-directory-p category-dir))))))

(ert-deftest test-module-files-have-plus-prefix ()
  "Test that all module files have + prefix."
  (skip-unless (file-directory-p test-v3-modules-dir))
  
  (let ((all-files (directory-files-recursively test-v3-modules-dir "\\.\\(org\\|el\\)$"))
        (violations '()))
    
    (dolist (file all-files)
      (let ((basename (file-name-nondirectory file)))
        ;; Skip .gitkeep and other non-module files
        (unless (or (string-prefix-p "." basename)
                    (string-prefix-p "+" basename))
          (push file violations))))
    
    ;; Report violations if any
    (when violations
      (message "Files without + prefix found:")
      (dolist (v violations)
        (message "  %s" v)))
    
    ;; All module files should have + prefix
    (should (null violations))))

(ert-deftest test-lang-modules-not-nested ()
  "Test that language modules are not nested (no backend/frontend subdirs).

This validates Requirement 4.2: 语言模块放在 modules/lang/ 下
（不再区分 backend/frontend）"
  (skip-unless (file-directory-p test-v3-modules-dir))
  
  (let ((lang-dir (expand-file-name "lang" test-v3-modules-dir)))
    (when (file-directory-p lang-dir)
      ;; Check that there are no backend/frontend subdirectories
      (should-not (file-directory-p (expand-file-name "backend" lang-dir)))
      (should-not (file-directory-p (expand-file-name "frontend" lang-dir)))
      
      ;; Check that lang modules exist directly under lang/
      (let ((lang-modules (directory-files lang-dir t "\\+.*\\.org$")))
        (should (> (length lang-modules) 0))))))

(ert-deftest test-core-directory-structure ()
  "Test that core directory has correct structure.

This validates Requirement 4.3: 核心模块放在 core/ 下且数量不超过 3 个"
  (let ((core-dir (expand-file-name "core" test-v3-root-dir)))
    (skip-unless (file-directory-p core-dir))
    
    ;; Count core module files (excluding generated .el files)
    (let ((core-org-files (directory-files core-dir t "\\.org$")))
      ;; Should have at most 3 core modules
      (should (<= (length core-org-files) 3)))))

(ert-deftest test-private-directory-exists ()
  "Test that private directory exists for user configuration.

This validates Requirement 4.4: 支持 private/ 目录"
  (let ((private-dir (expand-file-name "private" test-v3-root-dir)))
    (should (file-directory-p private-dir))))

;;; ============================================================
;;; Summary Statistics
;;; ============================================================

(ert-deftest test-directory-structure-summary ()
  "Print summary of directory structure for verification."
  (skip-unless (file-directory-p test-v3-modules-dir))
  
  (let ((categories (directory-files test-v3-modules-dir t "^[^.]"))
        (total-modules 0)
        (category-counts '()))
    
    (dolist (category-dir categories)
      (when (file-directory-p category-dir)
        (let* ((category-name (file-name-nondirectory category-dir))
               (module-files (directory-files category-dir t "\\+.*\\.org$"))
               (count (length module-files)))
          (push (cons category-name count) category-counts)
          (setq total-modules (+ total-modules count)))))
    
    (message "\n=== V3 Directory Structure Summary ===")
    (message "Total modules: %d" total-modules)
    (message "Categories:")
    (dolist (cat (nreverse category-counts))
      (message "  %s: %d modules" (car cat) (cdr cat)))
    
    ;; This test always passes, it's just for information
    (should t)))

(provide 'test-v3-directory-structure)
;;; test-v3-directory-structure.el ends here
