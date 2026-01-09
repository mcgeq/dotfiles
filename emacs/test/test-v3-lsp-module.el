;;; test-v3-lsp-module.el --- Tests for V3 LSP module -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 LSP module (+lsp).
;; Tests verify that LSP keybindings are unified across all language modes.

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
;;; Property 9: LSP 快捷键统一性
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 9: LSP 快捷键统一性**
;; **Validates: Requirements 5.3**
;;
;; Property: For any language mode with LSP enabled, M-. must be bound
;; to jump-to-definition and M-, must be bound to return.

(ert-deftest test-property-9-lsp-module-file-exists ()
  "Property 9 (prerequisite): LSP module file exists

*For any* LSP configuration, the init-lsp.el module file must exist.

**Validates: Requirements 5.1**"
  (let ((lsp-source-dir (expand-file-name "lang" test-v3-source-modules-dir))
        (lsp-modules-dir (expand-file-name "lang" test-v3-modules-dir)))
    ;; Check that lang modules source directory exists
    (should (file-directory-p lsp-source-dir))
    
    ;; Check that init-lsp.org exists (source)
    (let ((lsp-org (expand-file-name "init-lsp.org" lsp-source-dir)))
      (should (file-exists-p lsp-org)))
    
    ;; Check that tangled .el file exists
    (let ((lsp-el (expand-file-name "init-lsp.el" lsp-modules-dir)))
      (should (file-exists-p lsp-el)))))

(ert-deftest test-property-9-lsp-keymap-defines-navigation-keys ()
  "Property 9: LSP 快捷键统一性 - Navigation keys

*For any* 启用了 LSP 的语言模式，M-. 必须绑定到跳转定义，M-, 必须绑定到返回

**Validates: Requirements 5.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    ;; Only run if all required files exist
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      ;; Load dependencies first
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Load the LSP module (without actually initializing lsp-bridge)
      ;; We need to mock mcg-require-extension to avoid loading lsp-bridge
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check that mcg-lsp-keymap is defined
      (should (boundp 'mcg-lsp-keymap))
      
      ;; Check that M-. is bound to lsp-bridge-find-def
      (let ((m-dot-binding (assoc "M-." mcg-lsp-keymap)))
        (should m-dot-binding)
        (should (eq (cdr m-dot-binding) 'lsp-bridge-find-def)))
      
      ;; Check that M-, is bound to lsp-bridge-find-def-return
      (let ((m-comma-binding (assoc "M-," mcg-lsp-keymap)))
        (should m-comma-binding)
        (should (eq (cdr m-comma-binding) 'lsp-bridge-find-def-return))))))

(ert-deftest test-property-9-lsp-keymap-defines-reference-key ()
  "Property 9: LSP 快捷键统一性 - Reference key

*For any* 启用了 LSP 的语言模式，M-? 必须绑定到查找引用

**Validates: Requirements 5.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check that M-? is bound to lsp-bridge-find-references
      (let ((m-question-binding (assoc "M-?" mcg-lsp-keymap)))
        (should m-question-binding)
        (should (eq (cdr m-question-binding) 'lsp-bridge-find-references))))))

(ert-deftest test-property-9-lsp-keymap-completeness ()
  "Property 9: LSP 快捷键统一性 - All required keys defined

*For any* LSP keymap, all essential navigation keys must be defined.

**Validates: Requirements 5.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Required keys that must be in the keymap
      (let ((required-keys '("M-."    ; jump to definition
                             "M-,"    ; return from definition
                             "M-?"    ; find references
                             "C-c r r" ; rename
                             "C-c r f" ; format
                             )))
        (dolist (key required-keys)
          (should (assoc key mcg-lsp-keymap)))))))

(ert-deftest test-property-9-lsp-setup-keybindings-function-exists ()
  "Property 9: LSP keybinding setup function exists

*For any* LSP module, the mcg-lsp-setup-keybindings function must exist.

**Validates: Requirements 5.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check that the keybinding setup function exists
      (should (fboundp 'mcg-lsp-setup-keybindings)))))

;;; ============================================================
;;; Unit Tests for LSP Module
;;; ============================================================

(ert-deftest test-lsp-module-structure ()
  "Test that +lsp module has correct structure."
  (let ((lsp-org (expand-file-name "modules/lang/+lsp.org" test-v3-source-dir)))
    (when (file-exists-p lsp-org)
      (with-temp-buffer
        (insert-file-contents lsp-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\+lsp\\.el" content))
          (should (string-match-p "lexical-binding: t" content))
          
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          
          ;; Should provide +lsp
          (should (string-match-p "(provide '\\+lsp)" content))
          
          ;; Should have LSP server mapping
          (should (string-match-p "mcg-lsp-servers" content))
          
          ;; Should have unified keymap
          (should (string-match-p "mcg-lsp-keymap" content))
          
          ;; Should have setup function
          (should (string-match-p "mcg-lsp-setup" content))
          
          ;; Should have status command
          (should (string-match-p "mcg-lsp-status" content))
          
          ;; Should have format on save
          (should (string-match-p "mcg-lsp-format-on-save" content)))))))

(ert-deftest test-lsp-module-provides-feature ()
  "Test that +lsp module provides the +lsp feature."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check that +lsp feature is provided
      (should (featurep '+lsp)))))

(ert-deftest test-lsp-servers-mapping-defined ()
  "Test that LSP servers mapping is defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check that mcg-lsp-servers is defined
      (should (boundp 'mcg-lsp-servers))
      
      ;; Check that it contains expected language modes
      (should (assq 'rust-mode mcg-lsp-servers))
      (should (assq 'python-mode mcg-lsp-servers))
      (should (assq 'typescript-mode mcg-lsp-servers)))))

(ert-deftest test-lsp-format-on-save-variables-defined ()
  "Test that format-on-save variables are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check variables are defined
      (should (boundp 'mcg-lsp-format-on-save-modes))
      (should (boundp 'mcg-lsp-format-on-save-enabled))
      (should (boundp 'mcg-lsp-organize-imports-modes)))))

(ert-deftest test-lsp-utility-functions-defined ()
  "Test that LSP utility functions are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check functions are defined
      (should (fboundp 'mcg-lsp-setup))
      (should (fboundp 'mcg-lsp-status))
      (should (fboundp 'mcg-lsp-format-buffer))
      (should (fboundp 'mcg-lsp-organize-imports))
      (should (fboundp 'mcg-lsp-restart))
      (should (fboundp 'mcg/toggle-format-on-save))
      (should (fboundp 'mcg/lsp-help))
      (should (fboundp 'mcg/lsp-project-info)))))

(ert-deftest test-lsp-commands-are-interactive ()
  "Test that LSP commands are interactive."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check commands are interactive
      (should (commandp 'mcg-lsp-status))
      (should (commandp 'mcg-lsp-format-buffer))
      (should (commandp 'mcg-lsp-organize-imports))
      (should (commandp 'mcg-lsp-restart))
      (should (commandp 'mcg/toggle-format-on-save))
      (should (commandp 'mcg/lsp-help))
      (should (commandp 'mcg/lsp-project-info)))))

(ert-deftest test-lsp-aliases-defined ()
  "Test that LSP quick aliases are defined."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (lsp-file (expand-file-name "modules/lang/+lsp.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p lsp-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (name &optional feature) t))
                ((symbol-function 'mcg-log)
                 (lambda (&rest args) nil))
                ((symbol-function 'global-lsp-bridge-mode)
                 (lambda (&optional arg) nil)))
        (load lsp-file nil t))
      
      ;; Check aliases are defined
      (should (fboundp 'lsp-format))
      (should (fboundp 'lsp-imports))
      (should (fboundp 'lsp-restart))
      (should (fboundp 'lsp-status))
      (should (fboundp 'lsp-project))
      (should (fboundp 'lsp-help)))))

(provide 'test-v3-lsp-module)
;;; test-v3-lsp-module.el ends here
