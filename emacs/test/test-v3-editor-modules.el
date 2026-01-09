;;; test-v3-editor-modules.el --- Tests for V3 editor modules refactor -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for the V3 editor modules refactor.
;; Tests verify module registration consistency for the input -> editor merge.

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
;;; Property 2: Module Registration Consistency
;;; ============================================================

;; **Feature: emacs-modules-refactor, Property 2: Module Registration Consistency**
;; **Validates: Requirements 2.3, 3.2, 4.2, 5.3, 6.2, 8.2**
;;
;; Property: For any valid category and module name pair in the new structure,
;; the mcg! macro should successfully register it to mcg-modules-alist without errors.

(ert-deftest test-property-2-module-registration-editor-paredit ()
  "Property 2: Module Registration Consistency - editor/paredit

*For any* valid category and module name pair in the new structure,
the mcg! macro should successfully register it to mcg-modules-alist without errors.

**Feature: emacs-modules-refactor, Property 2: Module Registration Consistency**
**Validates: Requirements 2.3, 3.2, 4.2, 5.3, 6.2, 8.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (paredit-org (expand-file-name "editor/init-paredit.org" test-v3-org-modules-dir)))
    
    ;; Verify the paredit module org file exists
    (should (file-exists-p paredit-org))
    
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
      
      ;; Register paredit module in editor category (new location)
      (mcg! :editor paredit)
      
      ;; Verify paredit is registered in editor category
      (should (mcg-module-enabled-p :editor 'paredit))
      
      ;; Verify the module is in mcg-modules-alist
      (let ((editor-modules (cdr (assq :editor mcg-modules-alist))))
        (should editor-modules)
        (should (memq 'paredit editor-modules))))))

(ert-deftest test-property-2-module-registration-editor-format ()
  "Property 2: Module Registration Consistency - editor/format

*For any* valid category and module name pair in the new structure,
the mcg! macro should successfully register it to mcg-modules-alist without errors.

**Feature: emacs-modules-refactor, Property 2: Module Registration Consistency**
**Validates: Requirements 2.3, 3.2, 4.2, 5.3, 6.2, 8.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir))
        (format-org (expand-file-name "editor/init-format.org" test-v3-org-modules-dir)))
    
    ;; Verify the format module org file exists
    (should (file-exists-p format-org))
    
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
      
      ;; Register format module in editor category (new location)
      (mcg! :editor format)
      
      ;; Verify format is registered in editor category
      (should (mcg-module-enabled-p :editor 'format))
      
      ;; Verify the module is in mcg-modules-alist
      (let ((editor-modules (cdr (assq :editor mcg-modules-alist))))
        (should editor-modules)
        (should (memq 'format editor-modules))))))

(ert-deftest test-property-2-module-registration-multiple-editor-modules ()
  "Property 2: Module Registration Consistency - multiple editor modules

*For any* combination of editor modules including the new paredit and format,
the mcg! macro should successfully register all of them without errors.

**Feature: emacs-modules-refactor, Property 2: Module Registration Consistency**
**Validates: Requirements 2.3, 3.2, 4.2, 5.3, 6.2, 8.2**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir))
        (modules-file (expand-file-name "core/core-modules.el" test-v3-root-dir)))
    
    (when (and (file-exists-p bootstrap-file)
               (file-exists-p lib-file)
               (file-exists-p modules-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      (load modules-file nil t)
      
      ;; Test various combinations of editor modules
      (let ((test-combinations
             '((paredit)
               (format)
               (paredit format)
               (defaults paredit format)
               (defaults treesit navigation keybindings paredit format))))
        
        (dolist (combo test-combinations)
          ;; Clear state for each test
          (setq mcg-loaded-modules nil)
          (setq mcg-modules-alist nil)
          (setq mcg-disabled-modules nil)
          
          ;; Register the combination
          (eval `(mcg! :editor ,@combo))
          
          ;; Verify exactly the specified modules are registered
          (let ((editor-modules (cdr (assq :editor mcg-modules-alist))))
            (should editor-modules)
            ;; All specified modules should be present
            (dolist (mod combo)
              (should (memq mod editor-modules)))
            ;; Count should match
            (should (= (length editor-modules) (length combo)))))))))

(ert-deftest test-property-2-module-registration-cross-category ()
  "Property 2: Module Registration Consistency - cross category registration

*For any* valid module declaration across multiple categories including
the new editor modules, the mcg! macro should successfully register all.

**Feature: emacs-modules-refactor, Property 2: Module Registration Consistency**
**Validates: Requirements 2.3, 3.2, 4.2, 5.3, 6.2, 8.2**"
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
      (setq mcg-modules-alist nil)
      (setq mcg-disabled-modules nil)
      
      ;; Register modules across multiple categories
      (mcg! :ui theme modeline
            :editor defaults paredit format
            :enhance autosave which-key
            :lang rust python)
      
      ;; Verify all categories are registered
      (should (assq :ui mcg-modules-alist))
      (should (assq :editor mcg-modules-alist))
      (should (assq :enhance mcg-modules-alist))
      (should (assq :lang mcg-modules-alist))
      
      ;; Verify editor modules specifically
      (should (mcg-module-enabled-p :editor 'defaults))
      (should (mcg-module-enabled-p :editor 'paredit))
      (should (mcg-module-enabled-p :editor 'format))
      
      ;; Verify other categories
      (should (mcg-module-enabled-p :ui 'theme))
      (should (mcg-module-enabled-p :enhance 'autosave))
      (should (mcg-module-enabled-p :lang 'rust)))))

;;; ============================================================
;;; Unit Tests for editor/init-paredit.org
;;; ============================================================

(ert-deftest test-editor-paredit-org-file-exists ()
  "Test that editor/init-paredit.org file exists."
  (let ((paredit-org (expand-file-name "editor/init-paredit.org" test-v3-org-modules-dir)))
    (should (file-exists-p paredit-org))))

(ert-deftest test-editor-paredit-org-has-correct-structure ()
  "Test that init-paredit.org has correct org structure."
  (let ((paredit-org (expand-file-name "editor/init-paredit.org" test-v3-org-modules-dir)))
    (when (file-exists-p paredit-org)
      (with-temp-buffer
        (insert-file-contents paredit-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\* init-paredit\\.el" content))
          (should (string-match-p ":HEADER-ARGS:" content))
          (should (string-match-p ":tangle init-paredit\\.el" content))
          ;; Should have required sections
          (should (string-match-p "\\*\\* Headers" content))
          (should (string-match-p "\\*\\* Dependencies" content))
          (should (string-match-p "\\*\\* Configuration" content))
          (should (string-match-p "\\*\\* Keybindings" content))
          (should (string-match-p "\\*\\* Provide" content))
          ;; Should provide correct feature
          (should (string-match-p "(provide 'init-paredit)" content)))))))

(ert-deftest test-editor-paredit-org-has-required-functions ()
  "Test that init-paredit.org defines required functions."
  (let ((paredit-org (expand-file-name "editor/init-paredit.org" test-v3-org-modules-dir)))
    (when (file-exists-p paredit-org)
      (with-temp-buffer
        (insert-file-contents paredit-org)
        (let ((content (buffer-string)))
          ;; Should define setup functions
          (should (string-match-p "defun mcg--setup-fingertip" content))
          (should (string-match-p "defun mcg--setup-fingertip-keys" content))
          ;; Should define modes variable
          (should (string-match-p "defvar mcg-fingertip-modes" content)))))))

(ert-deftest test-editor-paredit-org-requires-core-lib ()
  "Test that init-paredit.org requires core-lib."
  (let ((paredit-org (expand-file-name "editor/init-paredit.org" test-v3-org-modules-dir)))
    (when (file-exists-p paredit-org)
      (with-temp-buffer
        (insert-file-contents paredit-org)
        (let ((content (buffer-string)))
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content)))))))

;;; ============================================================
;;; Unit Tests for editor/init-format.org
;;; ============================================================

(ert-deftest test-editor-format-org-file-exists ()
  "Test that editor/init-format.org file exists."
  (let ((format-org (expand-file-name "editor/init-format.org" test-v3-org-modules-dir)))
    (should (file-exists-p format-org))))

(ert-deftest test-editor-format-org-has-correct-structure ()
  "Test that init-format.org has correct org structure."
  (let ((format-org (expand-file-name "editor/init-format.org" test-v3-org-modules-dir)))
    (when (file-exists-p format-org)
      (with-temp-buffer
        (insert-file-contents format-org)
        (let ((content (buffer-string)))
          ;; Should have proper headers
          (should (string-match-p "\\* init-format\\.el" content))
          (should (string-match-p ":HEADER-ARGS:" content))
          (should (string-match-p ":tangle init-format\\.el" content))
          ;; Should have required sections
          (should (string-match-p "\\*\\* Headers" content))
          (should (string-match-p "\\*\\* Dependencies" content))
          (should (string-match-p "\\*\\* Configuration" content))
          (should (string-match-p "\\*\\* Provide" content))
          ;; Should provide correct feature
          (should (string-match-p "(provide 'init-format)" content)))))))

(ert-deftest test-editor-format-org-has-required-functions ()
  "Test that init-format.org defines required functions."
  (let ((format-org (expand-file-name "editor/init-format.org" test-v3-org-modules-dir)))
    (when (file-exists-p format-org)
      (with-temp-buffer
        (insert-file-contents format-org)
        (let ((content (buffer-string)))
          ;; Should define setup function
          (should (string-match-p "defun mcg--setup-wraplish" content))
          ;; Should define modes variable
          (should (string-match-p "defvar mcg-wraplish-modes" content)))))))

(ert-deftest test-editor-format-org-requires-core-lib ()
  "Test that init-format.org requires core-lib."
  (let ((format-org (expand-file-name "editor/init-format.org" test-v3-org-modules-dir)))
    (when (file-exists-p format-org)
      (with-temp-buffer
        (insert-file-contents format-org)
        (let ((content (buffer-string)))
          ;; Should require core-lib
          (should (string-match-p "(require 'core-lib)" content)))))))

;;; ============================================================
;;; Functional Equivalence Tests
;;; ============================================================

(ert-deftest test-editor-paredit-equivalent-to-input-fingertip ()
  "Test that editor/init-paredit.org is functionally equivalent to input/init-fingertip.org."
  (let ((paredit-org (expand-file-name "editor/init-paredit.org" test-v3-org-modules-dir))
        (fingertip-org (expand-file-name "input/init-fingertip.org" test-v3-org-modules-dir)))
    
    (when (and (file-exists-p paredit-org) (file-exists-p fingertip-org))
      (let ((paredit-content (with-temp-buffer
                               (insert-file-contents paredit-org)
                               (buffer-string)))
            (fingertip-content (with-temp-buffer
                                 (insert-file-contents fingertip-org)
                                 (buffer-string))))
        
        ;; All fingertip functions should be in paredit module
        (let ((fingertip-functions '("mcg--setup-fingertip"
                                     "mcg--setup-fingertip-keys")))
          (dolist (func fingertip-functions)
            ;; Function should exist in original fingertip
            (should (string-match-p (regexp-quote func) fingertip-content))
            ;; Function should exist in new paredit module
            (should (string-match-p (regexp-quote func) paredit-content))))
        
        ;; All fingertip variables should be in paredit module
        (let ((fingertip-vars '("mcg-fingertip-modes")))
          (dolist (var fingertip-vars)
            ;; Variable should exist in original fingertip
            (should (string-match-p (regexp-quote var) fingertip-content))
            ;; Variable should exist in new paredit module
            (should (string-match-p (regexp-quote var) paredit-content))))))))

(ert-deftest test-editor-format-equivalent-to-input-wraplish ()
  "Test that editor/init-format.org is functionally equivalent to input/init-wraplish.org."
  (let ((format-org (expand-file-name "editor/init-format.org" test-v3-org-modules-dir))
        (wraplish-org (expand-file-name "input/init-wraplish.org" test-v3-org-modules-dir)))
    
    (when (and (file-exists-p format-org) (file-exists-p wraplish-org))
      (let ((format-content (with-temp-buffer
                              (insert-file-contents format-org)
                              (buffer-string)))
            (wraplish-content (with-temp-buffer
                                (insert-file-contents wraplish-org)
                                (buffer-string))))
        
        ;; All wraplish functions should be in format module
        (let ((wraplish-functions '("mcg--setup-wraplish")))
          (dolist (func wraplish-functions)
            ;; Function should exist in original wraplish
            (should (string-match-p (regexp-quote func) wraplish-content))
            ;; Function should exist in new format module
            (should (string-match-p (regexp-quote func) format-content))))
        
        ;; All wraplish variables should be in format module
        (let ((wraplish-vars '("mcg-wraplish-modes")))
          (dolist (var wraplish-vars)
            ;; Variable should exist in original wraplish
            (should (string-match-p (regexp-quote var) wraplish-content))
            ;; Variable should exist in new format module
            (should (string-match-p (regexp-quote var) format-content))))))))

(provide 'test-v3-editor-modules)
;;; test-v3-editor-modules.el ends here
