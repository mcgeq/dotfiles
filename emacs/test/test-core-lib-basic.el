;;; test-core-lib-basic.el --- Basic tests for core-lib -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic tests to verify core-lib loads and functions correctly.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-root-dir
  (expand-file-name "../" (file-name-directory (or load-file-name buffer-file-name))))

(defvar test-config-dir
  (expand-file-name "site-lisp/config" test-root-dir))

;; Add to load path
(add-to-list 'load-path (expand-file-name "core" test-config-dir))

;; Load modules
(require 'core-bootstrap)
(require 'core-lib)

;; Initialize paths
(mcg-init-paths test-root-dir)
(mcg-lib-init)

;;; ============================================================
;;; Basic Loading Tests
;;; ============================================================

(ert-deftest test-core-lib-loads ()
  "Test that core-lib loads successfully."
  (should (featurep 'core-lib)))

(ert-deftest test-core-lib-cache-initialized ()
  "Test that extension cache is initialized."
  (should (hash-table-p mcg-extension-cache)))

(ert-deftest test-core-lib-keybinding-registry-initialized ()
  "Test that keybinding registry is initialized."
  (should (hash-table-p mcg-keybinding-registry)))

;;; ============================================================
;;; Cache System Tests
;;; ============================================================

(ert-deftest test-cache-store-and-lookup ()
  "Test cache store and lookup operations."
  (mcg-cache-store "test-key" "test-value")
  (should (equal (mcg-cache-lookup "test-key") "test-value")))

(ert-deftest test-cache-remove ()
  "Test cache remove operation."
  (mcg-cache-store "remove-test" "value")
  (should (mcg-cache-lookup "remove-test"))
  (mcg-cache-remove "remove-test")
  (should-not (mcg-cache-lookup "remove-test")))

(ert-deftest test-cache-clear ()
  "Test cache clear operation."
  (mcg-cache-store "clear-test-1" "value1")
  (mcg-cache-store "clear-test-2" "value2")
  (mcg-cache-clear)
  (should-not (mcg-cache-lookup "clear-test-1"))
  (should-not (mcg-cache-lookup "clear-test-2")))

;;; ============================================================
;;; Extension Discovery Tests
;;; ============================================================

(ert-deftest test-extension-discovery ()
  "Test that extensions can be discovered."
  (let ((extensions (mcg-list-extensions t)))
    ;; Should return a list
    (should (listp extensions))
    ;; If extensions directory exists and has content, should find some
    (when (and mcg-extensions-dir
               (file-directory-p mcg-extensions-dir)
               (directory-files mcg-extensions-dir nil "^[^.]"))
      (should (> (length extensions) 0)))))

(ert-deftest test-extension-path-lookup ()
  "Test extension path lookup."
  ;; First discover extensions
  (let ((extensions (mcg-list-extensions t)))
    (when extensions
      ;; Should be able to look up the first extension
      (let* ((first-ext (car extensions))
             (name (car first-ext))
             (expected-path (cdr first-ext)))
        (should (equal (mcg-extension-path name) expected-path))))))

(ert-deftest test-extension-exists-p ()
  "Test extension existence check."
  ;; Non-existent extension should return nil
  (should-not (mcg-extension-exists-p "non-existent-extension-xyz")))

;;; ============================================================
;;; Keybinding Registry Tests
;;; ============================================================

(ert-deftest test-keybinding-registration ()
  "Test keybinding registration."
  ;; Clear any previous state
  (setq mcg-keybinding-registry (make-hash-table :test 'equal))
  (setq mcg-keybinding-conflicts nil)
  
  ;; Register a binding
  (mcg-bind-key "C-c t t" 'test-command "test-module")
  
  ;; Should be in registry
  (let ((binding (mcg-lookup-keybinding "C-c t t")))
    (should binding)
    (should (eq (car binding) 'test-command))
    (should (equal (cdr binding) "test-module"))))

(ert-deftest test-keybinding-conflict-detection ()
  "Test keybinding conflict detection."
  ;; Clear any previous state
  (setq mcg-keybinding-registry (make-hash-table :test 'equal))
  (setq mcg-keybinding-conflicts nil)
  
  ;; Register first binding
  (mcg-bind-key "C-c t c" 'command-1 "module-1")
  
  ;; Register conflicting binding
  (mcg-bind-key "C-c t c" 'command-2 "module-2")
  
  ;; Should have detected conflict
  (should mcg-keybinding-conflicts)
  (should (= (length mcg-keybinding-conflicts) 1))
  
  ;; New binding should be in registry
  (let ((binding (mcg-lookup-keybinding "C-c t c")))
    (should (eq (car binding) 'command-2))))

(ert-deftest test-keybinding-unbind ()
  "Test keybinding unbind."
  ;; Clear any previous state
  (setq mcg-keybinding-registry (make-hash-table :test 'equal))
  
  ;; Register and then unbind
  (mcg-bind-key "C-c t u" 'test-unbind "test")
  (should (mcg-lookup-keybinding "C-c t u"))
  (mcg-unbind-key "C-c t u")
  (should-not (mcg-lookup-keybinding "C-c t u")))

(ert-deftest test-list-keybindings ()
  "Test listing keybindings."
  ;; Clear any previous state
  (setq mcg-keybinding-registry (make-hash-table :test 'equal))
  
  ;; Register some bindings
  (mcg-bind-key "C-c l 1" 'cmd-1 "mod-1")
  (mcg-bind-key "C-c l 2" 'cmd-2 "mod-2")
  
  (let ((bindings (mcg-list-keybindings)))
    (should (= (length bindings) 2))
    ;; Each binding should be (key command source)
    (dolist (b bindings)
      (should (= (length b) 3)))))

(ert-deftest test-key-normalization ()
  "Test key normalization."
  (should (equal (mcg-normalize-key "C-c a") "C-c a"))
  (should (stringp (mcg-normalize-key [?\C-c ?a]))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(when noninteractive
  (ert-run-tests-batch-and-exit))

(provide 'test-core-lib-basic)
;;; test-core-lib-basic.el ends here
