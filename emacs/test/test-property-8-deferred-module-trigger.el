;;; test-property-8-deferred-module-trigger.el --- Property test for deferred module trigger setup -*- lexical-binding: t; -*-

;;; Commentary:
;; **Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
;; **Validates: Requirements 4.2, 10.6**
;;
;; Property: For any language module registered in :lang category, it SHALL be
;; added to mcg-deferred-modules with appropriate mode hooks or file pattern triggers.
;;
;; Requirements:
;; - 4.2: WHEN a language module is registered THEN THE System SHALL set up
;;        deferred loading triggered by file extension or mode hook
;; - 10.6: WHEN opening a language file THEN THE System SHALL defer-load the
;;         corresponding language module

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Set up test environment
(defvar test-property-8-root-dir
  (expand-file-name "../site-lisp/config"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config.")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun test-property-8--load-core-modules ()
  "Load core modules for testing. Returns t if successful."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-property-8-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-property-8-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-property-8-root-dir)))
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      t)))

(defun test-property-8--clear-state ()
  "Clear module system state for clean testing."
  (setq mcg-deferred-modules nil)
  (setq mcg-loaded-modules nil)
  (setq mcg-modules-alist nil)
  (setq mcg-disabled-modules nil)
  (setq mcg-module-load-order nil))

(defun test-property-8--get-lang-modules-with-triggers ()
  "Get list of language modules that have triggers defined.
Returns list of module symbols."
  (let ((result nil))
    (dolist (entry mcg-module-triggers)
      (let ((mod-key (car entry)))
        (when (eq :lang (car mod-key))
          (push (cdr mod-key) result))))
    (nreverse result)))

;;; ============================================================
;;; Property 8: Deferred Module Trigger Setup
;;; ============================================================

;; **Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
;; **Validates: Requirements 4.2, 10.6**

(ert-deftest test-property-8-lang-modules-have-triggers-defined ()
  "Property 8: All :lang modules in mcg-module-triggers have valid triggers.

*For any* language module in mcg-module-triggers, it SHALL have a valid
trigger consisting of (PATTERN . MODE) where PATTERN is a file extension
regex and MODE is a major mode symbol.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    ;; Verify mcg-module-triggers is defined and non-empty
    (should (boundp 'mcg-module-triggers))
    (should mcg-module-triggers)
    
    ;; For each language module trigger entry
    (dolist (entry mcg-module-triggers)
      (let ((mod-key (car entry))
            (trigger (cdr entry)))
        (when (eq :lang (car mod-key))
          ;; Module key should be (:lang . MODULE)
          (should (keywordp (car mod-key)))
          (should (symbolp (cdr mod-key)))
          
          ;; Trigger should be (PATTERN . MODE)
          (should (consp trigger))
          (should (stringp (car trigger)))  ; File pattern
          (should (symbolp (cdr trigger)))  ; Major mode
          
          ;; Pattern should look like a file extension regex
          (should (string-match-p "\\\\\\." (car trigger))))))))

(ert-deftest test-property-8-setup-deferred-loading-registers-lang-modules ()
  "Property 8: mcg-setup-deferred-loading registers all :lang modules with triggers.

*For any* enabled :lang module that has a trigger defined in mcg-module-triggers,
calling mcg-setup-deferred-loading SHALL add it to mcg-deferred-modules.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    (test-property-8--clear-state)
    
    ;; Get all language modules that have triggers
    (let ((lang-modules-with-triggers (test-property-8--get-lang-modules-with-triggers)))
      (should lang-modules-with-triggers)
      
      ;; Register all language modules that have triggers
      (dolist (module lang-modules-with-triggers)
        ;; Build mcg! call dynamically
        (let ((cat-entry (assq :lang mcg-modules-alist)))
          (if cat-entry
              (setcdr cat-entry (append (cdr cat-entry) (list module)))
            (push (cons :lang (list module)) mcg-modules-alist)))
        (push (cons :lang module) mcg-module-load-order))
      
      ;; Setup deferred loading
      (mcg-setup-deferred-loading)
      
      ;; Verify all registered :lang modules with triggers are now deferred
      (dolist (module lang-modules-with-triggers)
        (let ((mod-key (cons :lang module)))
          (should (assoc mod-key mcg-deferred-modules)))))))

(ert-deftest test-property-8-deferred-module-has-correct-trigger ()
  "Property 8: Deferred modules have correct triggers from mcg-module-triggers.

*For any* :lang module added to mcg-deferred-modules via mcg-defer-module,
the trigger stored SHALL match the trigger defined in mcg-module-triggers.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    (test-property-8--clear-state)
    
    ;; Test with rust module (known to have a trigger)
    (mcg! :lang rust)
    (mcg-defer-module :lang 'rust)
    
    ;; Get the expected trigger from mcg-module-triggers
    (let* ((expected-trigger (cdr (assoc '(:lang . rust) mcg-module-triggers)))
           (deferred-entry (assoc '(:lang . rust) mcg-deferred-modules))
           (actual-trigger (cdr deferred-entry)))
      ;; Should have a deferred entry
      (should deferred-entry)
      ;; Trigger should match
      (should (equal expected-trigger actual-trigger)))))

(ert-deftest test-property-8-multiple-lang-modules-all-deferred ()
  "Property 8: Multiple :lang modules are all correctly deferred.

*For any* set of :lang modules registered via mcg!, calling
mcg-setup-deferred-loading SHALL defer all modules that have triggers.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    (test-property-8--clear-state)
    
    ;; Register multiple language modules
    (mcg! :lang rust python typescript web cpp lua zig)
    
    ;; Setup deferred loading
    (mcg-setup-deferred-loading)
    
    ;; All modules with triggers should be deferred
    (let ((modules-with-triggers '(rust python typescript web cpp lua zig)))
      (dolist (module modules-with-triggers)
        (let ((mod-key (cons :lang module)))
          ;; Check if this module has a trigger defined
          (when (assoc mod-key mcg-module-triggers)
            ;; It should be in deferred modules
            (should (assoc mod-key mcg-deferred-modules))))))))

(ert-deftest test-property-8-deferred-modules-not-immediately-loaded ()
  "Property 8: Deferred modules are NOT in mcg-loaded-modules after setup.

*For any* :lang module that is deferred, it SHALL NOT appear in
mcg-loaded-modules until its trigger fires.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    (test-property-8--clear-state)
    
    ;; Register language modules
    (mcg! :lang rust python typescript)
    
    ;; Setup deferred loading
    (mcg-setup-deferred-loading)
    
    ;; Verify deferred modules are NOT loaded
    (dolist (deferred-entry mcg-deferred-modules)
      (let ((mod-key (car deferred-entry)))
        ;; Should be deferred
        (should (assoc mod-key mcg-deferred-modules))
        ;; Should NOT be loaded
        (should-not (member mod-key mcg-loaded-modules))))))

(ert-deftest test-property-8-trigger-removes-from-deferred-list ()
  "Property 8: Triggering a deferred module removes it from mcg-deferred-modules.

*For any* deferred :lang module, when mcg--trigger-deferred-load is called,
the module SHALL be removed from mcg-deferred-modules.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    (test-property-8--clear-state)
    
    ;; Register and defer a module
    (mcg! :lang rust)
    (mcg-defer-module :lang 'rust)
    
    ;; Verify it's deferred
    (should (assoc '(:lang . rust) mcg-deferred-modules))
    
    ;; Trigger the deferred load (this will try to load the module file)
    ;; Note: The actual file may not exist, but the trigger mechanism should still work
    (mcg--trigger-deferred-load '(:lang . rust))
    
    ;; Should be removed from deferred list
    (should-not (assoc '(:lang . rust) mcg-deferred-modules))))

(ert-deftest test-property-8-file-pattern-triggers-are-valid-regex ()
  "Property 8: File pattern triggers are valid regex patterns.

*For any* trigger pattern in mcg-module-triggers, it SHALL be a valid
regex that can match file extensions.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    ;; Test each trigger pattern
    (dolist (entry mcg-module-triggers)
      (let* ((mod-key (car entry))
             (trigger (cdr entry))
             (pattern (car trigger)))
        (when (eq :lang (car mod-key))
          ;; Pattern should be a valid regex (should not error)
          (should (condition-case nil
                      (progn (string-match-p pattern "") t)
                    (error nil)))
          
          ;; Pattern should match expected file extensions
          (let ((module (cdr mod-key)))
            (cond
             ((eq module 'rust)
              (should (string-match-p pattern "test.rs")))
             ((eq module 'python)
              (should (string-match-p pattern "test.py")))
             ((eq module 'typescript)
              (should (or (string-match-p pattern "test.ts")
                          (string-match-p pattern "test.tsx"))))
             ((eq module 'web)
              (should (string-match-p pattern "test.vue")))
             ((eq module 'cpp)
              (should (or (string-match-p pattern "test.cpp")
                          (string-match-p pattern "test.h"))))
             ((eq module 'lua)
              (should (string-match-p pattern "test.lua")))
             ((eq module 'zig)
              (should (string-match-p pattern "test.zig"))))))))))

(ert-deftest test-property-8-disabled-lang-modules-not-deferred ()
  "Property 8: Disabled :lang modules are NOT added to deferred list.

*For any* :lang module that is disabled (prefixed with -), it SHALL NOT
be added to mcg-deferred-modules by mcg-setup-deferred-loading.

**Feature: modern-emacs-config, Property 8: Deferred Module Trigger Setup**
**Validates: Requirements 4.2, 10.6**"
  (when (test-property-8--load-core-modules)
    (test-property-8--clear-state)
    
    ;; Register with some disabled modules
    (mcg! :lang rust -python typescript -lua)
    
    ;; Setup deferred loading
    (mcg-setup-deferred-loading)
    
    ;; Enabled modules should be deferred (if they have triggers)
    (when (assoc '(:lang . rust) mcg-module-triggers)
      (should (assoc '(:lang . rust) mcg-deferred-modules)))
    (when (assoc '(:lang . typescript) mcg-module-triggers)
      (should (assoc '(:lang . typescript) mcg-deferred-modules)))
    
    ;; Disabled modules should NOT be deferred
    (should-not (assoc '(:lang . python) mcg-deferred-modules))
    (should-not (assoc '(:lang . lua) mcg-deferred-modules))))

(provide 'test-property-8-deferred-module-trigger)
;;; test-property-8-deferred-module-trigger.el ends here
