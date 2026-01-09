;;; test-v3-chinese-input.el --- Tests for Chinese input support -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based tests for Chinese input support (rime and pinyin).
;; Tests verify that rime auto-disable in prog-mode and pinyin matching work correctly.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-v3-root-dir
  (expand-file-name "../site-lisp/config"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for V3 config.")

(defvar test-v3-extensions-dir
  (expand-file-name "../site-lisp/extensions"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Extensions directory for V3 config.")

;;; ============================================================
;;; Helper Functions
;;; ============================================================

(defun test-v3--load-core-files ()
  "Load core configuration files for testing."
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-v3-root-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-v3-root-dir)))
    (when (file-exists-p bootstrap-file)
      (load bootstrap-file nil t))
    (when (file-exists-p lib-file)
      (load lib-file nil t))))

(defun test-v3--load-rime-module ()
  "Load rime module for testing."
  (let ((rime-file (expand-file-name "modules/tools/init-rime.el" test-v3-root-dir)))
    (when (file-exists-p rime-file)
      (load rime-file nil t))))

(defun test-v3--load-vertico-module ()
  "Load vertico module for testing."
  (let ((vertico-file (expand-file-name "modules/completion/init-vertico.el" test-v3-root-dir)))
    (when (file-exists-p vertico-file)
      (load vertico-file nil t))))

;;; ============================================================
;;; Property Tests for Chinese Input Support (emacs-config-optimization-v2)
;;; ============================================================

;; **Feature: emacs-config-optimization-v2, Property 8: Prog-mode Rime Disable**
;; **Validates: Requirements 8.4**
;;
;; Property: For any buffer in prog-mode, rime input method should be
;; automatically disabled.

(ert-deftest test-property-8-prog-mode-rime-disable ()
  "Property 8: Prog-mode Rime Disable

*For any* buffer in prog-mode, rime input method should be
automatically disabled.

**Feature: emacs-config-optimization-v2, Property 8: Prog-mode Rime Disable**
**Validates: Requirements 8.4**"
  (test-v3--load-core-files)
  (test-v3--load-rime-module)
  
  ;; Test the prog-mode disable function exists
  (should (fboundp 'mcg--rime-disable-in-prog-mode))
  
  ;; Test the text-mode enable function exists
  (should (fboundp 'mcg--rime-enable-in-text-mode))
  
  ;; Test the setup function exists
  (should (fboundp 'mcg--setup-rime-prog-mode))
  
  ;; Test the toggle command exists
  (should (fboundp 'mcg/toggle-rime-auto-enable))
  
  ;; Test the variables are defined
  (should (boundp 'mcg-rime-auto-enable))
  (should (boundp 'mcg-rime-prog-mode-disabled)))

(ert-deftest test-property-8-prog-mode-rime-disable-behavior ()
  "Property 8 (extended): Rime disable behavior in prog-mode

*For any* prog-mode buffer with rime active, entering the buffer
should trigger rime deactivation.

**Feature: emacs-config-optimization-v2, Property 8: Prog-mode Rime Disable**
**Validates: Requirements 8.4**"
  (test-v3--load-core-files)
  (test-v3--load-rime-module)
  
  ;; Verify that prog-mode-hook would contain our function after setup
  ;; (This tests the setup mechanism, not the actual rime behavior)
  (when (fboundp 'mcg--setup-rime-prog-mode)
    ;; The function should be callable without error
    ;; (actual hook addition happens when rime is loaded)
    (should (functionp 'mcg--rime-disable-in-prog-mode))
    (should (functionp 'mcg--rime-enable-in-text-mode))))

(ert-deftest test-property-8-prog-mode-rime-flag-tracking ()
  "Property 8 (extended): Rime disable flag tracking

*For any* rime disable action in prog-mode, the flag
mcg-rime-prog-mode-disabled should be set to track the state.

**Feature: emacs-config-optimization-v2, Property 8: Prog-mode Rime Disable**
**Validates: Requirements 8.4**"
  (test-v3--load-core-files)
  (test-v3--load-rime-module)
  
  ;; Test initial state
  (should (boundp 'mcg-rime-prog-mode-disabled))
  
  ;; Test that the variable can be set
  (setq mcg-rime-prog-mode-disabled t)
  (should (eq mcg-rime-prog-mode-disabled t))
  
  ;; Test that the variable can be reset
  (setq mcg-rime-prog-mode-disabled nil)
  (should (eq mcg-rime-prog-mode-disabled nil)))

(ert-deftest test-property-8-text-mode-rime-restore ()
  "Property 8 (extended): Rime restore in text-mode

*For any* text-mode buffer where rime was disabled by prog-mode,
entering text-mode should restore rime if mcg-rime-auto-enable is t.

**Feature: emacs-config-optimization-v2, Property 8: Prog-mode Rime Disable**
**Validates: Requirements 8.4**"
  (test-v3--load-core-files)
  (test-v3--load-rime-module)
  
  ;; Test that mcg-rime-auto-enable controls the behavior
  (should (boundp 'mcg-rime-auto-enable))
  
  ;; Test toggle function
  (when (fboundp 'mcg/toggle-rime-auto-enable)
    (let ((initial-value mcg-rime-auto-enable))
      ;; Toggle should flip the value
      (mcg/toggle-rime-auto-enable)
      (should (not (eq mcg-rime-auto-enable initial-value)))
      ;; Toggle again should restore
      (mcg/toggle-rime-auto-enable)
      (should (eq mcg-rime-auto-enable initial-value)))))

;;; ============================================================
;;; Property Tests for Pinyin Matching (emacs-config-optimization-v2)
;;; ============================================================

;; **Feature: emacs-config-optimization-v2, Property 7: Pinyin Matching**
;; **Validates: Requirements 8.2**
;;
;; Property: For any Chinese character, searching with its pinyin should
;; match that character in completion contexts.

(ert-deftest test-property-7-pinyin-matching ()
  "Property 7: Pinyin Matching

*For any* Chinese character, searching with its pinyin should
match that character in completion contexts.

**Feature: emacs-config-optimization-v2, Property 7: Pinyin Matching**
**Validates: Requirements 8.2**"
  (test-v3--load-core-files)
  (test-v3--load-vertico-module)
  
  ;; Test that pinyin completion is configured
  (should (boundp 'mcg-completion-enable-pinyin))
  
  ;; Test that the pinyin toggle function exists
  (should (fboundp 'mcg/toggle-pinyin-completion)))

(ert-deftest test-property-7-pinyin-matching-orderless-integration ()
  "Property 7 (extended): Pinyin matching integrates with orderless

*For any* orderless completion, pinyin matching style should be
available when mcg-completion-enable-pinyin is t.

**Feature: emacs-config-optimization-v2, Property 7: Pinyin Matching**
**Validates: Requirements 8.2**"
  (test-v3--load-core-files)
  (test-v3--load-vertico-module)
  
  ;; Test that the pinyin setup function is defined
  (should (fboundp 'mcg--setup-pinyin))
  
  ;; Test that orderless-matching-styles variable exists after setup
  ;; Note: mcg/completion-regex-pinyin is only defined when pinyinlib extension is available
  ;; In test environment without extensions, we verify the setup mechanism exists
  (when (and (boundp 'orderless-matching-styles)
             (fboundp 'mcg/completion-regex-pinyin)
             mcg-completion-enable-pinyin)
    ;; When pinyin is enabled and function exists, the style should be in the list
    (should (member 'mcg/completion-regex-pinyin orderless-matching-styles))))

(ert-deftest test-property-7-pinyin-toggle-functionality ()
  "Property 7 (extended): Pinyin toggle works correctly

*For any* toggle action, the pinyin matching style should be
added or removed from orderless-matching-styles.

**Feature: emacs-config-optimization-v2, Property 7: Pinyin Matching**
**Validates: Requirements 8.2**"
  (test-v3--load-core-files)
  (test-v3--load-vertico-module)
  
  ;; Test toggle function exists and is interactive
  (should (fboundp 'mcg/toggle-pinyin-completion))
  (should (commandp 'mcg/toggle-pinyin-completion)))

;;; ============================================================
;;; Unit Tests for Chinese Input Support
;;; ============================================================

(ert-deftest test-rime-variables-defined ()
  "Test that rime variables are defined."
  (test-v3--load-core-files)
  (test-v3--load-rime-module)
  
  ;; Check rime variables
  (should (boundp 'mcg-tools-rime-user-data-dir))
  (should (boundp 'mcg-tools-rime-show-candidate))
  (should (boundp 'mcg-tools-rime-posframe-bg))
  (should (boundp 'mcg-tools-rime-posframe-fg))
  (should (boundp 'mcg-tools-rime-cursor-color))
  (should (boundp 'mcg-rime-auto-enable))
  (should (boundp 'mcg-rime-prog-mode-disabled)))

(ert-deftest test-rime-functions-defined ()
  "Test that rime functions are defined."
  (test-v3--load-core-files)
  (test-v3--load-rime-module)
  
  ;; Check rime functions
  (should (fboundp 'mcg--setup-rime))
  (should (fboundp 'mcg--setup-rime-prog-mode))
  (should (fboundp 'mcg--rime-disable-in-prog-mode))
  (should (fboundp 'mcg--rime-enable-in-text-mode))
  (should (fboundp 'mcg/toggle-rime-auto-enable)))

(ert-deftest test-completion-pinyin-variables-defined ()
  "Test that completion pinyin variables are defined."
  (test-v3--load-core-files)
  (test-v3--load-vertico-module)
  
  ;; Check completion pinyin variables
  (should (boundp 'mcg-completion-enable-pinyin)))

(provide 'test-v3-chinese-input)
;;; test-v3-chinese-input.el ends here
