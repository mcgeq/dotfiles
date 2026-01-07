;;; test-v3-ui-modules.el --- Tests for V3 UI modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 UI modules (+theme, +modeline).
;; Tests verify that module files exist and provide correct features.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config-v3"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config (tangled .el files).")

(defvar test-v3-source-dir
  (expand-file-name "../config-org-v3"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Source directory for V3 config (.org files).")

(defvar test-v3-modules-dir
  (expand-file-name "modules" test-v3-root-dir)
  "Modules directory for V3 config (tangled .el files).")

(defvar test-v3-source-modules-dir
  (expand-file-name "modules" test-v3-source-dir)
  "Source modules directory for V3 config (.org files).")

;;; ============================================================
;;; Property 3: 模块文件存在性
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 3: 模块文件存在性**
;; **Validates: Requirements 3.1, 3.2**
;;
;; Property: For any module in mcg-modules-alist, the corresponding
;; configuration file must exist at modules/{category}/+{module}.el

(ert-deftest test-property-3-ui-module-files-exist ()
  "Property 3: 模块文件存在性

*For any* mcg-modules-alist 中的模块，对应的配置文件必须存在于
modules/{category}/+{module}.el 路径下

**Validates: Requirements 3.1, 3.2**"
  (let ((ui-source-dir (expand-file-name "ui" test-v3-source-modules-dir))
        (ui-modules-dir (expand-file-name "ui" test-v3-modules-dir)))
    ;; Check that UI modules source directory exists
    (should (file-directory-p ui-source-dir))
    
    ;; Check that +theme.org exists (source)
    (let ((theme-org (expand-file-name "+theme.org" ui-source-dir)))
      (should (file-exists-p theme-org)))
    
    ;; Check that +modeline.org exists (source)
    (let ((modeline-org (expand-file-name "+modeline.org" ui-source-dir)))
      (should (file-exists-p modeline-org)))
    
    ;; Check that tangled .el files exist
    (let ((theme-el (expand-file-name "+theme.el" ui-modules-dir)))
      (should (file-exists-p theme-el)))
    
    (let ((modeline-el (expand-file-name "+modeline.el" ui-modules-dir)))
      (should (file-exists-p modeline-el)))))

(ert-deftest test-property-3-ui-module-naming-convention ()
  "Property 3 (extended): UI modules follow naming convention

*For any* UI module file, it must be named with + prefix.

**Validates: Requirements 3.1**"
  (let ((ui-source-dir (expand-file-name "ui" test-v3-source-modules-dir)))
    (when (file-directory-p ui-source-dir)
      (let ((files (directory-files ui-source-dir nil "\\.org$")))
        ;; All .org files (except .gitkeep) should start with +
        (dolist (file files)
          (unless (string= file ".gitkeep")
            (should (string-prefix-p "+" file))))))))

(ert-deftest test-property-3-module-path-generation ()
  "Property 3 (extended): Module path generation is correct

*For any* category and module, mcg-module-path must generate
the correct path.

**Validates: Requirements 3.1**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test path generation for UI modules
      (let ((theme-path (mcg-module-path :ui 'theme)))
        (should theme-path)
        (should (string-match-p "ui/\\+theme\\.el$" theme-path)))
      
      (let ((modeline-path (mcg-module-path :ui 'modeline)))
        (should modeline-path)
        (should (string-match-p "ui/\\+modeline\\.el$" modeline-path))))))

;;; ============================================================
;;; Property 4: 模块 feature 提供正确性
;;; ============================================================

;; **Feature: emacs-config-refactor, Property 4: 模块 feature 提供正确性**
;; **Validates: Requirements 3.2**
;;
;; Property: For any loaded module file +{module}.el, the features list
;; must contain the +{module} symbol.

(ert-deftest test-property-4-theme-module-provides-feature ()
  "Property 4: 模块 feature 提供正确性 - +theme

*For any* 已加载的模块文件 +{module}.el，features 列表必须包含
+{module} 符号

**Validates: Requirements 3.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (theme-file (expand-file-name "modules/ui/+theme.el" test-v3-root-dir)))
    ;; Only run if all required files exist
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p theme-file))
      ;; Load dependencies first
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Load the theme module
      (load theme-file nil t)
      
      ;; Check that +theme feature is provided
      (should (featurep '+theme)))))

(ert-deftest test-property-4-modeline-module-provides-feature ()
  "Property 4: 模块 feature 提供正确性 - +modeline

*For any* 已加载的模块文件 +{module}.el，features 列表必须包含
+{module} 符号

**Validates: Requirements 3.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (modeline-file (expand-file-name "modules/ui/+modeline.el" test-v3-root-dir)))
    ;; Only run if all required files exist
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p modeline-file))
      ;; Load dependencies first
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Load the modeline module
      (load modeline-file nil t)
      
      ;; Check that +modeline feature is provided
      (should (featurep '+modeline)))))

(ert-deftest test-property-4-all-ui-modules-provide-features ()
  "Property 4 (extended): All UI modules provide their features

*For any* UI module file, loading it must provide the corresponding feature.

**Validates: Requirements 3.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (ui-modules-dir (expand-file-name "modules/ui" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-directory-p ui-modules-dir))
      ;; Load dependencies
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Find all .el files in ui directory
      (let ((el-files (directory-files ui-modules-dir t "\\+.*\\.el$")))
        (dolist (file el-files)
          (let* ((basename (file-name-base file))
                 (feature-name (intern basename)))
            ;; Load the module
            (load file nil t)
            ;; Check that feature is provided
            (should (featurep feature-name))))))))

;;; ============================================================
;;; Unit Tests for UI Modules
;;; ============================================================

(ert-deftest test-ui-theme-module-structure ()
  "Test that +theme module has correct structure."
  (let ((theme-org (expand-file-name "modules/ui/+theme.org" test-v3-source-dir)))
    (when (file-exists-p theme-org)
      (with-temp-buffer
        (insert-file-contents theme-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\+theme\\.el" content))
          (should (string-match-p "lexical-binding: t" content))
          
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          
          ;; Should provide +theme
          (should (string-match-p "(provide '\\+theme)" content))
          
          ;; Should have theme variables
          (should (string-match-p "mcg-ui-default-theme" content))
          (should (string-match-p "mcg-ui-light-theme" content))
          (should (string-match-p "mcg-ui-dark-theme" content))
          
          ;; Should have theme commands
          (should (string-match-p "mcg/switch-theme" content))
          (should (string-match-p "mcg/toggle-dark-light" content)))))))

(ert-deftest test-ui-modeline-module-structure ()
  "Test that +modeline module has correct structure."
  (let ((modeline-org (expand-file-name "modules/ui/+modeline.org" test-v3-source-dir)))
    (when (file-exists-p modeline-org)
      (with-temp-buffer
        (insert-file-contents modeline-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\+modeline\\.el" content))
          (should (string-match-p "lexical-binding: t" content))
          
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          
          ;; Should provide +modeline
          (should (string-match-p "(provide '\\+modeline)" content))
          
          ;; Should have modeline variables
          (should (string-match-p "mcg-modeline-position" content))
          (should (string-match-p "mcg-modeline-active-modules" content))
          
          ;; Should have awesome-tray setup
          (should (string-match-p "awesome-tray" content))
          
          ;; Should have modeline commands
          (should (string-match-p "mcg/toggle-modeline" content)))))))

(ert-deftest test-ui-theme-variables-defined ()
  "Test that theme module defines required variables."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (theme-file (expand-file-name "modules/ui/+theme.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p theme-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      (load theme-file nil t)
      
      ;; Check variables are defined
      (should (boundp 'mcg-ui-default-theme))
      (should (boundp 'mcg-ui-light-theme))
      (should (boundp 'mcg-ui-dark-theme))
      (should (boundp 'mcg-ui-auto-switch-theme))
      (should (boundp 'mcg-ui-theme-switch-hour)))))

(ert-deftest test-ui-theme-functions-defined ()
  "Test that theme module defines required functions."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (theme-file (expand-file-name "modules/ui/+theme.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p theme-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      (load theme-file nil t)
      
      ;; Check functions are defined
      (should (fboundp 'mcg-ui-setup-theme))
      (should (fboundp 'mcg/switch-theme))
      (should (fboundp 'mcg/toggle-dark-light))
      (should (fboundp 'mcg/reload-theme)))))

(ert-deftest test-ui-modeline-variables-defined ()
  "Test that modeline module defines required variables."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (modeline-file (expand-file-name "modules/ui/+modeline.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p modeline-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      (load modeline-file nil t)
      
      ;; Check variables are defined
      (should (boundp 'mcg-modeline-position))
      (should (boundp 'mcg-modeline-date-format))
      (should (boundp 'mcg-modeline-active-modules))
      (should (boundp 'mcg-modeline-use-second-line))
      (should (boundp 'mcg-modeline-buffer-name-max-length)))))

(ert-deftest test-ui-modeline-functions-defined ()
  "Test that modeline module defines required functions."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (modeline-file (expand-file-name "modules/ui/+modeline.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p modeline-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      (load modeline-file nil t)
      
      ;; Check functions are defined
      (should (fboundp 'mcg-modeline-setup-awesome-tray))
      (should (fboundp 'mcg/toggle-modeline))
      (should (fboundp 'mcg/modeline-add-module))
      (should (fboundp 'mcg/modeline-remove-module))
      (should (fboundp 'mcg/modeline-list-modules)))))

(ert-deftest test-ui-theme-commands-are-interactive ()
  "Test that theme commands are interactive."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (theme-file (expand-file-name "modules/ui/+theme.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p theme-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      (load theme-file nil t)
      
      ;; Check commands are interactive
      (should (commandp 'mcg/switch-theme))
      (should (commandp 'mcg/toggle-dark-light))
      (should (commandp 'mcg/reload-theme)))))

(ert-deftest test-ui-modeline-commands-are-interactive ()
  "Test that modeline commands are interactive."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (modeline-file (expand-file-name "modules/ui/+modeline.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p modeline-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      (load modeline-file nil t)
      
      ;; Check commands are interactive
      (should (commandp 'mcg/toggle-modeline))
      (should (commandp 'mcg/modeline-add-module))
      (should (commandp 'mcg/modeline-remove-module))
      (should (commandp 'mcg/modeline-list-modules)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(ert-deftest test-ui-modules-can-be-loaded-via-mcg-load-module ()
  "Test that UI modules can be loaded via mcg-load-module."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (theme-file (expand-file-name "modules/ui/+theme.el" test-v3-root-dir))
        (modeline-file (expand-file-name "modules/ui/+modeline.el" test-v3-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file)
               (file-exists-p theme-file)
               (file-exists-p modeline-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Clear loaded modules
      (setq mcg-loaded-modules nil)
      
      ;; Register UI modules
      (mcg! :ui theme modeline)
      
      ;; Check modules are registered
      (should (mcg-module-enabled-p :ui 'theme))
      (should (mcg-module-enabled-p :ui 'modeline))
      
      ;; Check module paths are correct
      (should (file-exists-p (mcg-module-path :ui 'theme)))
      (should (file-exists-p (mcg-module-path :ui 'modeline))))))

(provide 'test-v3-ui-modules)
;;; test-v3-ui-modules.el ends here
