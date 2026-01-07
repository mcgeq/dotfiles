;;; test-v3-core-modules.el --- Tests for V3 core-modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 core-modules module.
;; Tests verify that the mcg! macro correctly parses module declarations
;; and that module disabling works correctly.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config-v3"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config.")

;;; ============================================================
;;; Property Tests
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 1: mcg! 宏解析正确性**
;; **Validates: Requirements 1.1, 1.2**
;;
;; Property: For any mcg! declaration with :category module combinations,
;; the parsed mcg-modules-alist must contain (category . module) entries.

(ert-deftest test-property-1-mcg!-macro-parsing-correctness ()
  "Property 1: mcg! 宏解析正确性

*For any* mcg! 声明中的 :category module 组合，
解析后 mcg-modules-alist 必须包含 (category . module) 条目

**Validates: Requirements 1.1, 1.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test case 1: Single category with single module
      (mcg! :ui theme)
      (should (mcg-module-enabled-p :ui 'theme))
      (should-not (mcg-module-enabled-p :editor 'defaults))
      
      ;; Test case 2: Single category with multiple modules
      (mcg! :ui theme modeline)
      (should (mcg-module-enabled-p :ui 'theme))
      (should (mcg-module-enabled-p :ui 'modeline))
      
      ;; Test case 3: Multiple categories
      (mcg! :ui theme modeline
            :editor defaults treesit)
      (should (mcg-module-enabled-p :ui 'theme))
      (should (mcg-module-enabled-p :ui 'modeline))
      (should (mcg-module-enabled-p :editor 'defaults))
      (should (mcg-module-enabled-p :editor 'treesit))
      
      ;; Test case 4: Complex declaration
      (mcg! :ui theme modeline
            :editor defaults treesit navigation
            :completion vertico
            :lang lsp rust python)
      (should (mcg-module-enabled-p :ui 'theme))
      (should (mcg-module-enabled-p :ui 'modeline))
      (should (mcg-module-enabled-p :editor 'defaults))
      (should (mcg-module-enabled-p :editor 'treesit))
      (should (mcg-module-enabled-p :editor 'navigation))
      (should (mcg-module-enabled-p :completion 'vertico))
      (should (mcg-module-enabled-p :lang 'lsp))
      (should (mcg-module-enabled-p :lang 'rust))
      (should (mcg-module-enabled-p :lang 'python)))))

(ert-deftest test-property-1-mcg!-macro-category-registration ()
  "Property 1 (extended): Categories are correctly registered

*For any* category in mcg! declaration, the category must appear
as a key in mcg-modules-alist.

**Validates: Requirements 1.1, 1.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test that categories are registered
      (mcg! :ui theme
            :editor defaults
            :lang rust)
      
      ;; Check that all categories exist in mcg-modules-alist
      (should (assq :ui mcg-modules-alist))
      (should (assq :editor mcg-modules-alist))
      (should (assq :lang mcg-modules-alist))
      
      ;; Check that modules are in the correct categories
      (should (memq 'theme (cdr (assq :ui mcg-modules-alist))))
      (should (memq 'defaults (cdr (assq :editor mcg-modules-alist))))
      (should (memq 'rust (cdr (assq :lang mcg-modules-alist)))))))

;; **Feature: emacs-config-refactor, Property 2: 模块禁用语法正确性**
;; **Validates: Requirements 1.3**
;;
;; Property: For any mcg! declaration with :category -module syntax,
;; the module must be marked as disabled and not loaded.

(ert-deftest test-property-2-module-disable-syntax-correctness ()
  "Property 2: 模块禁用语法正确性

*For any* mcg! 声明中的 :category -module 语法，
该模块必须被标记为禁用且不会被加载

**Validates: Requirements 1.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test case 1: Single disabled module
      (mcg! :lang rust -lua)
      (should (mcg-module-enabled-p :lang 'rust))
      (should (mcg-module-disabled-p :lang 'lua))
      (should-not (mcg-module-enabled-p :lang 'lua))
      
      ;; Test case 2: Multiple disabled modules
      (mcg! :lang lsp rust -lua -zig)
      (should (mcg-module-enabled-p :lang 'lsp))
      (should (mcg-module-enabled-p :lang 'rust))
      (should (mcg-module-disabled-p :lang 'lua))
      (should (mcg-module-disabled-p :lang 'zig))
      (should-not (mcg-module-enabled-p :lang 'lua))
      (should-not (mcg-module-enabled-p :lang 'zig))
      
      ;; Test case 3: Disabled modules across categories
      (mcg! :ui theme -modeline
            :editor defaults -treesit
            :lang rust -lua)
      (should (mcg-module-enabled-p :ui 'theme))
      (should (mcg-module-disabled-p :ui 'modeline))
      (should (mcg-module-enabled-p :editor 'defaults))
      (should (mcg-module-disabled-p :editor 'treesit))
      (should (mcg-module-enabled-p :lang 'rust))
      (should (mcg-module-disabled-p :lang 'lua)))))

(ert-deftest test-property-2-disabled-modules-recorded ()
  "Property 2 (extended): Disabled modules are recorded in mcg-disabled-modules

*For any* disabled module, it must appear in mcg-disabled-modules.

**Validates: Requirements 1.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Disable some modules
      (mcg! :lang rust -lua -zig
            :ui theme -modeline)
      
      ;; Check mcg-disabled-modules contains the disabled modules
      (let ((lang-disabled (cdr (assq :lang mcg-disabled-modules)))
            (ui-disabled (cdr (assq :ui mcg-disabled-modules))))
        (should lang-disabled)
        (should ui-disabled)
        (should (memq 'lua lang-disabled))
        (should (memq 'zig lang-disabled))
        (should (memq 'modeline ui-disabled))))))

;;; ============================================================
;;; Unit Tests for core-modules
;;; ============================================================

(ert-deftest test-core-modules-provides-feature ()
  "Test that core-modules provides its feature."
  (let ((modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (file-exists-p modules-file)
      (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
            (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
        (when (file-exists-p bootstrap-file)
          (load bootstrap-file nil t))
        (when (file-exists-p lib-file)
          (load lib-file nil t)))
      (load modules-file nil t)
      (should (featurep 'core-modules)))))

(ert-deftest test-core-modules-variables-defined ()
  "Test that module system variables are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Check that all required variables are defined
      (should (boundp 'mcg-modules-alist))
      (should (boundp 'mcg-disabled-modules))
      (should (boundp 'mcg-module-load-order))
      (should (boundp 'mcg-loaded-modules))
      (should (boundp 'mcg-module-load-errors)))))

(ert-deftest test-core-modules-functions-defined ()
  "Test that module system functions are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Check that all required functions are defined
      (should (fboundp 'mcg-module-path))
      (should (fboundp 'mcg-module-exists-p))
      (should (fboundp 'mcg-module-enabled-p))
      (should (fboundp 'mcg-module-disabled-p))
      (should (fboundp 'mcg-load-module))
      (should (fboundp 'mcg-load-modules-by-category))
      (should (fboundp 'mcg-load-all-modules))
      (should (fboundp 'mcg-list-modules)))))

(ert-deftest test-mcg-module-path-function ()
  "Test mcg-module-path function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test path generation
      (let ((path (mcg-module-path :ui 'theme)))
        (should path)
        (should (stringp path))
        (should (string-match-p "ui/\\+theme\\.el$" path)))
      
      (let ((path (mcg-module-path :lang 'rust)))
        (should path)
        (should (string-match-p "lang/\\+rust\\.el$" path))))))

(ert-deftest test-mcg!-clears-previous-state ()
  "Test that mcg! clears previous module registrations."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; First declaration
      (mcg! :ui theme modeline
            :editor defaults)
      (should (mcg-module-enabled-p :ui 'theme))
      (should (mcg-module-enabled-p :editor 'defaults))
      
      ;; Second declaration should clear previous state
      (mcg! :lang rust python)
      (should (mcg-module-enabled-p :lang 'rust))
      (should (mcg-module-enabled-p :lang 'python))
      ;; Previous modules should no longer be enabled
      (should-not (mcg-module-enabled-p :ui 'theme))
      (should-not (mcg-module-enabled-p :editor 'defaults)))))

(ert-deftest test-mcg--parse-modules-function ()
  "Test the internal mcg--parse-modules function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test parsing
      (let ((result (mcg--parse-modules '(:ui theme modeline :lang rust -lua))))
        (should result)
        (should (listp result))
        ;; Check :ui category
        (let ((ui-entry (assq :ui result)))
          (should ui-entry)
          (should (assq 'theme (cdr ui-entry)))
          (should (assq 'modeline (cdr ui-entry)))
          ;; Both should be enabled
          (should (cdr (assq 'theme (cdr ui-entry))))
          (should (cdr (assq 'modeline (cdr ui-entry)))))
        ;; Check :lang category
        (let ((lang-entry (assq :lang result)))
          (should lang-entry)
          (should (assq 'rust (cdr lang-entry)))
          (should (assq 'lua (cdr lang-entry)))
          ;; rust enabled, lua disabled
          (should (cdr (assq 'rust (cdr lang-entry))))
          (should-not (cdr (assq 'lua (cdr lang-entry)))))))))

(ert-deftest test-mcg-list-enabled-modules ()
  "Test mcg-list-enabled-modules function."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (mcg! :ui theme modeline
            :lang rust -lua)
      
      (let ((enabled (mcg-list-enabled-modules)))
        (should enabled)
        (should (listp enabled))
        ;; Should contain enabled modules
        (should (member (cons :ui 'theme) enabled))
        (should (member (cons :ui 'modeline) enabled))
        (should (member (cons :lang 'rust) enabled))
        ;; Should not contain disabled modules
        (should-not (member (cons :lang 'lua) enabled))))))

;;; ============================================================
;;; Property 7: 文件类型触发语言模块加载
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 7: 文件类型触发语言模块加载**
;; **Validates: Requirements 2.2**
;;
;; Property: For any language module with a file type trigger,
;; opening a file of that type must trigger the module to load.

(ert-deftest test-property-7-file-type-triggers-module-load ()
  "Property 7: 文件类型触发语言模块加载

*For any* 语言模块对应的文件类型，打开该类型文件后，
语言模块必须被加载

**Validates: Requirements 2.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Clear state
      (setq mcg-deferred-modules nil)
      (setq mcg-loaded-modules nil)
      
      ;; Register language modules
      (mcg! :lang rust python typescript)
      
      ;; Verify triggers are defined for these modules
      (let ((rust-trigger (assoc '(:lang . rust) mcg-module-triggers))
            (python-trigger (assoc '(:lang . python) mcg-module-triggers))
            (typescript-trigger (assoc '(:lang . typescript) mcg-module-triggers)))
        ;; All language modules should have triggers defined
        (should rust-trigger)
        (should python-trigger)
        (should typescript-trigger)
        
        ;; Verify trigger patterns
        (should (string-match-p "\\.rs" (car (cdr rust-trigger))))
        (should (string-match-p "\\.py" (car (cdr python-trigger))))
        (should (string-match-p "\\.tsx?" (car (cdr typescript-trigger))))))))

(ert-deftest test-property-7-defer-module-records-trigger ()
  "Property 7 (extended): mcg-defer-module records the trigger

*For any* deferred module, the trigger must be recorded in mcg-deferred-modules.

**Validates: Requirements 2.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Clear state
      (setq mcg-deferred-modules nil)
      (setq mcg-loaded-modules nil)
      
      ;; Register and defer a module
      (mcg! :lang rust)
      (mcg-defer-module :lang 'rust)
      
      ;; Check that the module is recorded in mcg-deferred-modules
      (let ((deferred-entry (assoc '(:lang . rust) mcg-deferred-modules)))
        (should deferred-entry)
        ;; Should have a trigger
        (should (cdr deferred-entry))))))

(ert-deftest test-property-7-setup-deferred-loading ()
  "Property 7 (extended): mcg-setup-deferred-loading sets up all triggers

*For any* enabled language module with a trigger, mcg-setup-deferred-loading
must set up the deferred loading.

**Validates: Requirements 2.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Clear state
      (setq mcg-deferred-modules nil)
      (setq mcg-loaded-modules nil)
      
      ;; Register language modules
      (mcg! :lang rust python)
      
      ;; Setup deferred loading
      (mcg-setup-deferred-loading)
      
      ;; Check that modules are deferred
      (should (assoc '(:lang . rust) mcg-deferred-modules))
      (should (assoc '(:lang . python) mcg-deferred-modules)))))

;;; ============================================================
;;; Deferred Loading Unit Tests
;;; ============================================================

(ert-deftest test-deferred-loading-variables-defined ()
  "Test that deferred loading variables are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Check deferred loading variables
      (should (boundp 'mcg-deferred-modules))
      (should (boundp 'mcg-module-triggers))
      (should (boundp 'mcg-module-load-hook)))))

(ert-deftest test-deferred-loading-functions-defined ()
  "Test that deferred loading functions are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Check deferred loading functions
      (should (fboundp 'mcg-defer-module))
      (should (fboundp 'mcg--trigger-deferred-load))
      (should (fboundp 'mcg-setup-deferred-loading))
      (should (fboundp 'mcg-list-deferred-modules)))))

(ert-deftest test-mcg-module-triggers-structure ()
  "Test that mcg-module-triggers has correct structure."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Check structure of mcg-module-triggers
      (should mcg-module-triggers)
      (should (listp mcg-module-triggers))
      
      ;; Each entry should be ((CATEGORY . MODULE) . (PATTERN . MODE))
      (dolist (entry mcg-module-triggers)
        (let ((mod-key (car entry))
              (trigger (cdr entry)))
          ;; mod-key should be (CATEGORY . MODULE)
          (should (consp mod-key))
          (should (keywordp (car mod-key)))
          (should (symbolp (cdr mod-key)))
          ;; trigger should be (PATTERN . MODE)
          (should (consp trigger))
          (should (stringp (car trigger)))
          (should (symbolp (cdr trigger))))))))

(ert-deftest test-mcg-after!-macro-with-feature ()
  "Test mcg-after! macro with feature symbol."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test that mcg-after! expands correctly for feature symbol
      (let ((expansion (macroexpand '(mcg-after! magit
                                       (setq test-var t)))))
        (should expansion)
        ;; Should expand to with-eval-after-load or eval-after-load
        (should (memq (car expansion) '(with-eval-after-load eval-after-load)))))))

;;; ============================================================
;;; mcg-list-modules Command Tests
;;; ============================================================

(ert-deftest test-mcg-list-modules-command-exists ()
  "Test that mcg-list-modules command exists and is interactive."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Check that mcg-list-modules is defined and interactive
      (should (fboundp 'mcg-list-modules))
      (should (commandp 'mcg-list-modules)))))

(ert-deftest test-mcg-list-modules-creates-buffer ()
  "Test that mcg-list-modules creates the *MCG Modules* buffer."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Register some modules
      (mcg! :ui theme modeline
            :lang rust -lua)
      
      ;; Call mcg-list-modules
      (mcg-list-modules)
      
      ;; Check that the buffer was created
      (should (get-buffer "*MCG Modules*"))
      
      ;; Check buffer content
      (with-current-buffer "*MCG Modules*"
        (let ((content (buffer-string)))
          ;; Should contain header
          (should (string-match-p "MCG 模块列表" content))
          ;; Should contain enabled modules
          (should (string-match-p "theme" content))
          (should (string-match-p "modeline" content))
          (should (string-match-p "rust" content))))
      
      ;; Clean up
      (kill-buffer "*MCG Modules*"))))

(provide 'test-v3-core-modules)
;;; test-v3-core-modules.el ends here
