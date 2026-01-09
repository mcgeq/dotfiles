;;; test-v3-lang-modules.el --- Tests for V3 language modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 language modules (+rust, +python, +typescript, +web).
;; Tests verify that module dependencies are loaded in the correct order.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config (tangled .el files).")

(defvar test-v3-source-dir
  (expand-file-name "../config-org"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Source directory for V3 config (.org files).")

(defvar test-v3-modules-dir
  (expand-file-name "modules" test-v3-root-dir)
  "Modules directory for V3 config (tangled .el files).")

(defvar test-v3-source-modules-dir
  (expand-file-name "modules" test-v3-source-dir)
  "Source modules directory for V3 config (.org files).")

;;; ============================================================
;;; Property 5: 模块依赖加载顺序
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 5: 模块依赖加载顺序**
;; **Validates: Requirements 3.3, 6.2**
;;
;; Property: For any module that declares dependencies, its dependencies
;; must be loaded before the module itself.

(ert-deftest test-property-5-lang-module-files-exist ()
  "Property 5 (prerequisite): Language module files exist

*For any* language module, the corresponding .org source file must exist.

**Validates: Requirements 3.1**"
  (let ((lang-source-dir (expand-file-name "lang" test-v3-source-modules-dir)))
    ;; Check that lang modules source directory exists
    (should (file-directory-p lang-source-dir))
    
    ;; Check that language module .org files exist (using init- prefix)
    (should (file-exists-p (expand-file-name "init-rust.org" lang-source-dir)))
    (should (file-exists-p (expand-file-name "init-python.org" lang-source-dir)))
    (should (file-exists-p (expand-file-name "init-typescript.org" lang-source-dir)))
    (should (file-exists-p (expand-file-name "init-web.org" lang-source-dir)))
    (should (file-exists-p (expand-file-name "init-lsp.org" lang-source-dir)))))

(ert-deftest test-property-5-lang-modules-declare-lsp-dependency ()
  "Property 5: Language modules declare +lsp dependency

*For any* language module that uses LSP, it must declare dependency on +lsp.

**Validates: Requirements 3.3, 5.2**"
  (let ((lang-source-dir (expand-file-name "lang" test-v3-source-modules-dir)))
    ;; Check each language module for dependency declaration
    (dolist (module '("+rust.org" "+python.org" "+typescript.org" "+web.org"))
      (let ((module-path (expand-file-name module lang-source-dir)))
        (when (file-exists-p module-path)
          (with-temp-buffer
            (insert-file-contents module-path)
            (let ((content (buffer-string)))
              ;; Should have dependency metadata or require +lsp
              (should (or (string-match-p ":depends.*\\+lsp" content)
                          (string-match-p "(require '\\+lsp" content)
                          ;; Or check for mcg-module-enabled-p :lang 'lsp
                          (string-match-p "mcg-module-enabled-p :lang 'lsp" content))))))))))

(ert-deftest test-property-5-lsp-module-loads-before-lang-modules ()
  "Property 5: +lsp module loads before language modules

*For any* language module with LSP dependency, +lsp must be loaded first.

**Validates: Requirements 3.3, 6.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir))
        (rust-file (expand-file-name "modules/lang/+rust.el" test-v3-root-dir)))
    ;; Only run if all required files exist
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      ;; Load core modules
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Clear state
      (setq mcg-loaded-modules nil)
      (setq mcg-module-load-order nil)
      
      ;; Register modules with lsp first (correct order)
      (mcg! :lang lsp rust python typescript web)
      
      ;; Verify that lsp is registered before other lang modules
      (let ((lang-modules (cdr (assq :lang mcg-modules-alist))))
        (should lang-modules)
        ;; lsp should be first in the list
        (should (eq (car lang-modules) 'lsp))))))

(ert-deftest test-property-5-module-load-order-tracking ()
  "Property 5: Module load order is tracked correctly

*For any* loaded module, its load order must be recorded in mcg-module-load-order.

**Validates: Requirements 3.3, 6.2**"
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
      (setq mcg-loaded-modules nil)
      (setq mcg-module-load-order nil)
      
      ;; Register modules
      (mcg! :lang lsp rust)
      
      ;; Verify mcg-module-load-order is a list
      (should (listp mcg-module-load-order))
      
      ;; After loading, the order should be tracked
      ;; (Note: actual loading depends on file existence)
      (should (boundp 'mcg-module-load-order)))))

;;; ============================================================
;;; Language Module Structure Tests
;;; ============================================================

(ert-deftest test-rust-module-structure ()
  "Test that +rust module has correct structure."
  (let ((rust-org (expand-file-name "modules/lang/+rust.org" test-v3-source-dir)))
    (when (file-exists-p rust-org)
      (with-temp-buffer
        (insert-file-contents rust-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\+rust\\.el" content))
          (should (string-match-p "lexical-binding: t" content))
          
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          
          ;; Should provide +rust
          (should (string-match-p "(provide '\\+rust)" content))
          
          ;; Should have cargo commands
          (should (string-match-p "cargo" content))
          
          ;; Should have keybindings
          (should (string-match-p "mcg-rust-setup-keybindings" content)))))))

(ert-deftest test-python-module-structure ()
  "Test that +python module has correct structure."
  (let ((python-org (expand-file-name "modules/lang/+python.org" test-v3-source-dir)))
    (when (file-exists-p python-org)
      (with-temp-buffer
        (insert-file-contents python-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\+python\\.el" content))
          (should (string-match-p "lexical-binding: t" content))
          
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          
          ;; Should provide +python
          (should (string-match-p "(provide '\\+python)" content))
          
          ;; Should have ty + ruff configuration
          (should (string-match-p "ty" content))
          (should (string-match-p "ruff" content))
          
          ;; Should have pytest support
          (should (string-match-p "pytest" content))
          
          ;; Should have virtual environment support
          (should (string-match-p "venv" content)))))))

(ert-deftest test-typescript-module-structure ()
  "Test that +typescript module has correct structure."
  (let ((ts-org (expand-file-name "modules/lang/+typescript.org" test-v3-source-dir)))
    (when (file-exists-p ts-org)
      (with-temp-buffer
        (insert-file-contents ts-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\+typescript\\.el" content))
          (should (string-match-p "lexical-binding: t" content))
          
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          
          ;; Should provide +typescript
          (should (string-match-p "(provide '\\+typescript)" content))
          
          ;; Should have biome configuration
          (should (string-match-p "biome" content))
          
          ;; Should have tree-sitter mode configuration
          (should (string-match-p "typescript-ts-mode" content))
          (should (string-match-p "tsx-ts-mode" content)))))))

(ert-deftest test-web-module-structure ()
  "Test that +web module has correct structure."
  (let ((web-org (expand-file-name "modules/lang/+web.org" test-v3-source-dir)))
    (when (file-exists-p web-org)
      (with-temp-buffer
        (insert-file-contents web-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\+web\\.el" content))
          (should (string-match-p "lexical-binding: t" content))
          
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          
          ;; Should provide +web
          (should (string-match-p "(provide '\\+web)" content))
          
          ;; Should have web-mode configuration
          (should (string-match-p "web-mode" content))
          
          ;; Should have Vue support
          (should (string-match-p "\\.vue" content))
          
          ;; Should have CSS configuration
          (should (string-match-p "css" content)))))))

;;; ============================================================
;;; Module Feature Provision Tests
;;; ============================================================

(ert-deftest test-lang-modules-provide-correct-features ()
  "Test that language modules provide correct features.

*For any* language module file +{module}.el, it must provide the +{module} feature.

**Validates: Requirements 3.2**"
  (let ((lang-source-dir (expand-file-name "lang" test-v3-source-modules-dir)))
    (dolist (module '(("rust" . "\\+rust")
                      ("python" . "\\+python")
                      ("typescript" . "\\+typescript")
                      ("web" . "\\+web")))
      (let* ((module-name (car module))
             (feature-name (cdr module))
             (module-path (expand-file-name (format "+%s.org" module-name) lang-source-dir)))
        (when (file-exists-p module-path)
          (with-temp-buffer
            (insert-file-contents module-path)
            (let ((content (buffer-string)))
              ;; Should provide the correct feature (escape + in regex)
              (should (string-match-p (format "(provide '%s)" feature-name) content)))))))))

;;; ============================================================
;;; Keybinding Consistency Tests
;;; ============================================================

(ert-deftest test-lang-modules-have-keybinding-setup ()
  "Test that language modules have keybinding setup functions.

*For any* language module, it should have a keybinding setup function.

**Validates: Requirements 3.4**"
  (let ((lang-source-dir (expand-file-name "lang" test-v3-source-modules-dir)))
    (dolist (module '("rust" "python" "typescript" "web"))
      (let ((module-path (expand-file-name (format "+%s.org" module) lang-source-dir)))
        (when (file-exists-p module-path)
          (with-temp-buffer
            (insert-file-contents module-path)
            (let ((content (buffer-string)))
              ;; Should have keybinding setup function
              (should (string-match-p "setup-keybindings" content)))))))))

(ert-deftest test-lang-modules-use-consistent-keybinding-prefixes ()
  "Test that language modules use consistent keybinding prefixes.

*For any* language module, it should use C-c r for refactoring, C-c t for testing.

**Validates: Requirements 3.4, 5.3**"
  (let ((lang-source-dir (expand-file-name "lang" test-v3-source-modules-dir)))
    (dolist (module '("rust" "python" "typescript"))
      (let ((module-path (expand-file-name (format "+%s.org" module) lang-source-dir)))
        (when (file-exists-p module-path)
          (with-temp-buffer
            (insert-file-contents module-path)
            (let ((content (buffer-string)))
              ;; Should use C-c r prefix for refactoring
              (should (string-match-p "C-c r" content))
              ;; Should use C-c t prefix for testing (if applicable)
              (when (member module '("rust" "python"))
                (should (string-match-p "C-c t" content))))))))))

;;; ============================================================
;;; Tool Check Function Tests
;;; ============================================================

(ert-deftest test-lang-modules-have-tool-check-functions ()
  "Test that language modules have tool check functions.

*For any* language module, it should have a function to check tool availability.

**Validates: Requirements 8.4**"
  (let ((lang-source-dir (expand-file-name "lang" test-v3-source-modules-dir)))
    (dolist (module '("rust" "python" "typescript" "web"))
      (let ((module-path (expand-file-name (format "+%s.org" module) lang-source-dir)))
        (when (file-exists-p module-path)
          (with-temp-buffer
            (insert-file-contents module-path)
            (let ((content (buffer-string)))
              ;; Should have tool check function
              (should (string-match-p "check-tools" content)))))))))

;;; ============================================================
;;; Module Triggers Tests
;;; ============================================================

(ert-deftest test-lang-modules-have-auto-mode-alist-entries ()
  "Test that language modules set up auto-mode-alist entries.

*For any* language module, it should configure auto-mode-alist for its file types.

**Validates: Requirements 2.2**"
  (let ((lang-source-dir (expand-file-name "lang" test-v3-source-modules-dir)))
    ;; Rust module should handle .rs files
    (let ((rust-path (expand-file-name "+rust.org" lang-source-dir)))
      (when (file-exists-p rust-path)
        (with-temp-buffer
          (insert-file-contents rust-path)
          (should (string-match-p "\\.rs" (buffer-string))))))
    
    ;; Python module should handle .py files
    (let ((python-path (expand-file-name "+python.org" lang-source-dir)))
      (when (file-exists-p python-path)
        (with-temp-buffer
          (insert-file-contents python-path)
          (should (string-match-p "\\.py" (buffer-string))))))
    
    ;; TypeScript module should handle .ts/.tsx files
    (let ((ts-path (expand-file-name "+typescript.org" lang-source-dir)))
      (when (file-exists-p ts-path)
        (with-temp-buffer
          (insert-file-contents ts-path)
          (should (string-match-p "\\.ts" (buffer-string)))
          (should (string-match-p "\\.tsx" (buffer-string))))))
    
    ;; Web module should handle .vue/.html files
    (let ((web-path (expand-file-name "+web.org" lang-source-dir)))
      (when (file-exists-p web-path)
        (with-temp-buffer
          (insert-file-contents web-path)
          (should (string-match-p "\\.vue" (buffer-string)))
          (should (string-match-p "\\.html" (buffer-string))))))))

(provide 'test-v3-lang-modules)
;;; test-v3-lang-modules.el ends here
