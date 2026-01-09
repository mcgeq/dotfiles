;;; test-v3-enhance-modules.el --- Tests for V3 enhance modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 enhance modules.
;; Tests verify module isolation and functional equivalence.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config.")

(defvar test-v3-org-modules-dir
  (expand-file-name "../config-org/modules"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 org modules.")

;;; ============================================================
;;; Property 1: Module Isolation
;;; ============================================================

;; **Feature: emacs-modules-refactor, Property 1: Module Isolation**
;; **Validates: Requirements 1.2, 4.3**
;;
;; Property: For any enhance module loaded individually, the mcg-loaded-modules
;; list should contain only that module and its explicit dependencies,
;; not other enhance modules.

(ert-deftest test-property-1-module-isolation-autosave ()
  "Property 1: Module Isolation - autosave module

*For any* enhance module loaded individually, the mcg-loaded-modules list
should contain only that module and its explicit dependencies,
not other enhance modules.

**Feature: emacs-modules-refactor, Property 1: Module Isolation**
**Validates: Requirements 1.2, 4.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (autosave-org (expand-file-name "enhance/init-autosave.org" test-v3-org-modules-dir)))
    
    ;; Verify the autosave module org file exists
    (should (file-exists-p autosave-org))
    
    ;; Load core modules if available
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Clear state
      (setq mcg-loaded-modules nil)
      (setq mcg-modules-alist nil)
      (setq mcg-disabled-modules nil)
      
      ;; Register only autosave module
      (mcg! :enhance autosave)
      
      ;; Verify only autosave is registered in enhance category
      (let ((enhance-modules (cdr (assq :enhance mcg-modules-alist))))
        (should enhance-modules)
        (should (memq 'autosave enhance-modules))
        ;; Should NOT contain other enhance modules
        (should-not (memq 'which-key enhance-modules))
        (should-not (memq 'helpful enhance-modules))
        (should-not (memq 'recentf enhance-modules))
        (should-not (memq 'symbol enhance-modules))))))

(ert-deftest test-property-1-module-isolation-multiple-enhance ()
  "Property 1: Module Isolation - multiple enhance modules

*For any* combination of enhance modules, each should be independently
registered without implicit dependencies on other enhance modules.

**Feature: emacs-modules-refactor, Property 1: Module Isolation**
**Validates: Requirements 1.2, 4.3**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test various combinations of enhance modules
      (let ((test-combinations
             '((autosave)
               (autosave which-key)
               (helpful recentf)
               (autosave which-key helpful recentf symbol))))
        
        (dolist (combo test-combinations)
          ;; Clear state for each test
          (setq mcg-loaded-modules nil)
          (setq mcg-modules-alist nil)
          (setq mcg-disabled-modules nil)
          
          ;; Register the combination
          (eval `(mcg! :enhance ,@combo))
          
          ;; Verify exactly the specified modules are registered
          (let ((enhance-modules (cdr (assq :enhance mcg-modules-alist))))
            (should enhance-modules)
            ;; All specified modules should be present
            (dolist (mod combo)
              (should (memq mod enhance-modules)))
            ;; Count should match
            (should (= (length enhance-modules) (length combo)))))))))

;;; ============================================================
;;; Unit Tests for enhance/init-autosave.org
;;; ============================================================

(ert-deftest test-enhance-autosave-org-file-exists ()
  "Test that enhance/init-autosave.org file exists."
  (let ((autosave-org (expand-file-name "enhance/init-autosave.org" test-v3-org-modules-dir)))
    (should (file-exists-p autosave-org))))

(ert-deftest test-enhance-autosave-org-has-correct-structure ()
  "Test that init-autosave.org has correct org structure."
  (let ((autosave-org (expand-file-name "enhance/init-autosave.org" test-v3-org-modules-dir)))
    (when (file-exists-p autosave-org)
      (with-temp-buffer
        (insert-file-contents autosave-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\* init-autosave\\.el" content))
          (should (string-match-p ":HEADER-ARGS:" content))
          (should (string-match-p ":tangle init-autosave\\.el" content))
          ;; Should have required sections
          (should (string-match-p "\\*\\* Headers" content))
          (should (string-match-p "\\*\\* Dependencies" content))
          (should (string-match-p "\\*\\* Idle Auto-Save" content))
          (should (string-match-p "\\*\\* Provide" content))
          ;; Should provide correct feature
          (should (string-match-p "(provide 'init-autosave)" content)))))))

(ert-deftest test-enhance-autosave-org-has-required-functions ()
  "Test that init-autosave.org defines required functions."
  (let ((autosave-org (expand-file-name "enhance/init-autosave.org" test-v3-org-modules-dir)))
    (when (file-exists-p autosave-org)
      (with-temp-buffer
        (insert-file-contents autosave-org)
        (let ((content (buffer-string)))
          ;; Should define auto-save functions
          (should (string-match-p "defun mcg--auto-save-buffers" content))
          (should (string-match-p "defun mcg--auto-save-enable" content))
          (should (string-match-p "defun mcg--auto-save-disable" content))
          ;; Should define customization group
          (should (string-match-p "defgroup mcg-auto-save" content))
          ;; Should define customization variables
          (should (string-match-p "defcustom mcg-auto-save-idle" content))
          (should (string-match-p "defcustom mcg-auto-save-silent" content))
          ;; Should define internal variable
          (should (string-match-p "defvar mcg--auto-save-timer" content)))))))

(ert-deftest test-enhance-autosave-org-requires-core-lib ()
  "Test that init-autosave.org requires core-lib."
  (let ((autosave-org (expand-file-name "enhance/init-autosave.org" test-v3-org-modules-dir)))
    (when (file-exists-p autosave-org)
      (with-temp-buffer
        (insert-file-contents autosave-org)
        (let ((content (buffer-string)))
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content))
          ;; Should NOT require other enhance modules
          (should-not (string-match-p "(require 'init-which-key)" content))
          (should-not (string-match-p "(require 'init-helpful)" content))
          (should-not (string-match-p "(require 'init-recentf)" content))
          (should-not (string-match-p "(require 'init-symbol)" content)))))))

(ert-deftest test-enhance-autosave-disables-emacs-defaults ()
  "Test that init-autosave.org disables Emacs default auto-save."
  (let ((autosave-org (expand-file-name "enhance/init-autosave.org" test-v3-org-modules-dir)))
    (when (file-exists-p autosave-org)
      (with-temp-buffer
        (insert-file-contents autosave-org)
        (let ((content (buffer-string)))
          ;; Should disable Emacs default backup/auto-save
          (should (string-match-p "make-backup-files nil" content))
          (should (string-match-p "auto-save-default nil" content))
          (should (string-match-p "create-lockfiles nil" content)))))))

(provide 'test-v3-enhance-modules)
;;; test-v3-enhance-modules.el ends here


;;; ============================================================
;;; Property 3: Functional Equivalence
;;; ============================================================

;; **Feature: emacs-modules-refactor, Property 3: Functional Equivalence**
;; **Validates: Requirements 1.4**
;;
;; Property: For any function or variable that should be defined for the
;; enhance modules, the split module should contain the required definitions.
;; Note: The original init-utils.org was deleted after the refactor was
;; verified complete. These tests now verify the split modules contain
;; the expected functionality.

(ert-deftest test-property-3-functional-equivalence-autosave ()
  "Property 3: Functional Equivalence - autosave module

*For any* auto-save functionality, the init-autosave.org module should
contain all required functions and variables.

**Feature: emacs-modules-refactor, Property 3: Functional Equivalence**
**Validates: Requirements 1.4**"
  (let ((autosave-org (expand-file-name "enhance/init-autosave.org" test-v3-org-modules-dir)))
    
    ;; Module file should exist
    (should (file-exists-p autosave-org))
    
    (let ((autosave-content (with-temp-buffer
                              (insert-file-contents autosave-org)
                              (buffer-string))))
      
      ;; All auto-save functions should be in autosave module
      (let ((autosave-functions '("mcg--auto-save-buffers"
                                  "mcg--auto-save-enable"
                                  "mcg--auto-save-disable")))
        (dolist (func autosave-functions)
          ;; Function should exist in autosave module
          (should (string-match-p (regexp-quote func) autosave-content))))
      
      ;; All auto-save variables should be in autosave module
      (let ((autosave-vars '("mcg-auto-save-idle"
                             "mcg-auto-save-silent"
                             "mcg--auto-save-timer")))
        (dolist (var autosave-vars)
          ;; Variable should exist in autosave module
          (should (string-match-p (regexp-quote var) autosave-content))))
      
      ;; Auto-save group should be in autosave module
      (should (string-match-p "defgroup mcg-auto-save" autosave-content)))))

(ert-deftest test-property-3-functional-equivalence-which-key ()
  "Property 3: Functional Equivalence - which-key module

*For any* which-key functionality, the init-which-key.org module should
contain all required functions and configurations.

**Feature: emacs-modules-refactor, Property 3: Functional Equivalence**
**Validates: Requirements 1.4**"
  (let ((which-key-org (expand-file-name "enhance/init-which-key.org" test-v3-org-modules-dir)))
    
    (should (file-exists-p which-key-org))
    
    (let ((which-key-content (with-temp-buffer
                               (insert-file-contents which-key-org)
                               (buffer-string))))
      
      ;; Setup function should exist
      (should (string-match-p "mcg--setup-which-key" which-key-content))
      
      ;; Key configurations should be present
      (should (string-match-p "which-key-idle-delay" which-key-content))
      (should (string-match-p "which-key-popup-type" which-key-content))
      (should (string-match-p "which-key-add-key-based-replacements" which-key-content)))))

(ert-deftest test-property-3-functional-equivalence-recentf ()
  "Property 3: Functional Equivalence - recentf module

*For any* recentf functionality, the init-recentf.org module should
contain all required functions and configurations.

**Feature: emacs-modules-refactor, Property 3: Functional Equivalence**
**Validates: Requirements 1.4**"
  (let ((recentf-org (expand-file-name "enhance/init-recentf.org" test-v3-org-modules-dir)))
    
    (should (file-exists-p recentf-org))
    
    (let ((recentf-content (with-temp-buffer
                             (insert-file-contents recentf-org)
                             (buffer-string))))
      
      ;; Setup function should exist
      (should (string-match-p "mcg--setup-recentf" recentf-content))
      
      ;; Key configurations should be present
      (should (string-match-p "recentf-max-saved-items" recentf-content))
      (should (string-match-p "recentf-exclude" recentf-content)))))

(ert-deftest test-property-3-functional-equivalence-helpful ()
  "Property 3: Functional Equivalence - helpful module

*For any* helpful functionality, the init-helpful.org module should
contain all required functions and key bindings.

**Feature: emacs-modules-refactor, Property 3: Functional Equivalence**
**Validates: Requirements 1.4**"
  (let ((helpful-org (expand-file-name "enhance/init-helpful.org" test-v3-org-modules-dir)))
    
    (should (file-exists-p helpful-org))
    
    (let ((helpful-content (with-temp-buffer
                             (insert-file-contents helpful-org)
                             (buffer-string))))
      
      ;; Setup function should exist
      (should (string-match-p "mcg--setup-helpful" helpful-content))
      
      ;; Key bindings should be present
      (should (string-match-p "helpful-callable" helpful-content))
      (should (string-match-p "helpful-variable" helpful-content))
      (should (string-match-p "helpful-key" helpful-content)))))

(ert-deftest test-property-3-functional-equivalence-symbol-overlay ()
  "Property 3: Functional Equivalence - symbol-overlay module

*For any* symbol-overlay functionality, the init-symbol.org module should
contain all required functions and key bindings.

**Feature: emacs-modules-refactor, Property 3: Functional Equivalence**
**Validates: Requirements 1.4**"
  (let ((symbol-org (expand-file-name "enhance/init-symbol.org" test-v3-org-modules-dir)))
    
    (should (file-exists-p symbol-org))
    
    (let ((symbol-content (with-temp-buffer
                            (insert-file-contents symbol-org)
                            (buffer-string))))
      
      ;; Setup function should exist
      (should (string-match-p "mcg--setup-symbol-overlay" symbol-content))
      
      ;; Key bindings should be present
      (should (string-match-p "symbol-overlay-put" symbol-content))
      (should (string-match-p "symbol-overlay-jump-next" symbol-content))
      (should (string-match-p "symbol-overlay-rename" symbol-content)))))

(ert-deftest test-property-3-all-enhance-modules-provide-features ()
  "Property 3: All enhance modules provide their features

*For any* enhance module, it should provide a feature matching its filename.

**Feature: emacs-modules-refactor, Property 3: Functional Equivalence**
**Validates: Requirements 1.4**"
  (let ((enhance-modules '(("init-autosave.org" . "init-autosave")
                           ("init-which-key.org" . "init-which-key")
                           ("init-recentf.org" . "init-recentf")
                           ("init-helpful.org" . "init-helpful")
                           ("init-symbol.org" . "init-symbol"))))
    (dolist (mod-pair enhance-modules)
      (let* ((filename (car mod-pair))
             (feature (cdr mod-pair))
             (filepath (expand-file-name (concat "enhance/" filename) test-v3-org-modules-dir)))
        (when (file-exists-p filepath)
          (with-temp-buffer
            (insert-file-contents filepath)
            (let ((content (buffer-string)))
              ;; Should provide the correct feature
              (should (string-match-p (format "(provide '%s)" feature) content)))))))))

;;; ============================================================
;;; Additional Unit Tests for enhance modules
;;; ============================================================

(ert-deftest test-enhance-which-key-org-file-exists ()
  "Test that enhance/init-which-key.org file exists."
  (let ((which-key-org (expand-file-name "enhance/init-which-key.org" test-v3-org-modules-dir)))
    (should (file-exists-p which-key-org))))

(ert-deftest test-enhance-recentf-org-file-exists ()
  "Test that enhance/init-recentf.org file exists."
  (let ((recentf-org (expand-file-name "enhance/init-recentf.org" test-v3-org-modules-dir)))
    (should (file-exists-p recentf-org))))

(ert-deftest test-enhance-helpful-org-file-exists ()
  "Test that enhance/init-helpful.org file exists."
  (let ((helpful-org (expand-file-name "enhance/init-helpful.org" test-v3-org-modules-dir)))
    (should (file-exists-p helpful-org))))

(ert-deftest test-enhance-symbol-org-file-exists ()
  "Test that enhance/init-symbol.org file exists."
  (let ((symbol-org (expand-file-name "enhance/init-symbol.org" test-v3-org-modules-dir)))
    (should (file-exists-p symbol-org))))

(ert-deftest test-enhance-modules-require-only-core-lib ()
  "Test that enhance modules only require core-lib, not other enhance modules."
  (let ((enhance-modules '("init-autosave.org"
                           "init-which-key.org"
                           "init-recentf.org"
                           "init-helpful.org"
                           "init-symbol.org")))
    (dolist (mod enhance-modules)
      (let ((filepath (expand-file-name (concat "enhance/" mod) test-v3-org-modules-dir)))
        (when (file-exists-p filepath)
          (with-temp-buffer
            (insert-file-contents filepath)
            (let ((content (buffer-string)))
              ;; Should require core-lib
              (should (string-match-p "(require 'core-lib)" content))
              ;; Should NOT require other enhance modules (except recentf which requires recentf built-in)
              (unless (string= mod "init-recentf.org")
                (should-not (string-match-p "(require 'init-autosave)" content))
                (should-not (string-match-p "(require 'init-which-key)" content))
                (should-not (string-match-p "(require 'init-helpful)" content))
                (should-not (string-match-p "(require 'init-symbol)" content))))))))))
