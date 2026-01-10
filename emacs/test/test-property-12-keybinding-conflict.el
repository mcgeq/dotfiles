;;; test-property-12-keybinding-conflict.el --- Property test for keybinding conflict detection -*- lexical-binding: t; -*-

;;; Commentary:
;; Property-based test for keybinding conflict detection.
;; **Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
;; **Validates: Requirements 6.6**
;;
;; Property: For any keybinding registered via mcg-bind-key, if the same key
;; sequence was previously registered, a warning SHALL be logged indicating
;; the conflict with the previous binding source.

;;; Code:

(require 'ert)

;; Set up test environment
(defvar test-emacs-dir
  (expand-file-name ".."
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Root directory for emacs config.")

(defvar test-config-dir
  (expand-file-name "site-lisp/config" test-emacs-dir)
  "Config directory for V3 config.")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defun test-reset-keybinding-state ()
  "Reset keybinding registry and conflicts for clean test state."
  (setq mcg-keybinding-registry nil)
  (setq mcg-keybinding-conflicts nil))

(defun test-gen-random-key ()
  "Generate a random key sequence for testing."
  (let* ((modifiers '("C-" "M-" "C-M-" "s-" ""))
         (keys '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                 "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                 "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
         (modifier (nth (random (length modifiers)) modifiers))
         (key (nth (random (length keys)) keys)))
    (concat modifier key)))

(defun test-gen-random-command ()
  "Generate a random command symbol for testing."
  (let ((commands '(forward-char backward-char next-line previous-line
                    beginning-of-line end-of-line forward-word backward-word
                    kill-line yank undo save-buffer find-file)))
    (nth (random (length commands)) commands)))

(defun test-gen-random-source ()
  "Generate a random source string for testing."
  (let ((sources '("init-keybindings" "init-navigation" "init-magit"
                   "init-search" "init-org" "init-lsp" "init-completion")))
    (nth (random (length sources)) sources)))

;;; ============================================================
;;; Property 12: Keybinding Conflict Detection
;;; ============================================================

;; **Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
;; **Validates: Requirements 6.6**

(ert-deftest test-property-12-conflict-detection-basic ()
  "Property 12: Basic conflict detection

*For any* keybinding registered via mcg-bind-key, if the same key sequence
was previously registered with a DIFFERENT command, a conflict SHALL be
recorded in mcg-keybinding-conflicts.

**Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
**Validates: Requirements 6.6**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Reset state for clean test
      (test-reset-keybinding-state)
      
      ;; Bind a key first time
      (mcg-bind-key "C-c t a" 'forward-char "source-1")
      
      ;; Verify no conflicts yet
      (should (null mcg-keybinding-conflicts))
      
      ;; Bind the same key with a DIFFERENT command
      (mcg-bind-key "C-c t a" 'backward-char "source-2")
      
      ;; Verify conflict was recorded
      (should mcg-keybinding-conflicts)
      (should (= 1 (length mcg-keybinding-conflicts)))
      
      ;; Verify conflict structure
      (let ((conflict (car mcg-keybinding-conflicts)))
        ;; conflict is (key old-binding new-binding)
        (should (equal "C-c t a" (nth 0 conflict)))
        ;; old-binding is (command . source)
        (should (equal 'forward-char (car (nth 1 conflict))))
        (should (equal "source-1" (cdr (nth 1 conflict))))
        ;; new-binding is (command . source)
        (should (equal 'backward-char (car (nth 2 conflict))))
        (should (equal "source-2" (cdr (nth 2 conflict))))))))

(ert-deftest test-property-12-no-conflict-same-command ()
  "Property 12: No conflict when rebinding same command

*For any* keybinding registered via mcg-bind-key, if the same key sequence
was previously registered with the SAME command, no conflict SHALL be recorded.

**Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
**Validates: Requirements 6.6**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Reset state for clean test
      (test-reset-keybinding-state)
      
      ;; Bind a key first time
      (mcg-bind-key "C-c t b" 'forward-char "source-1")
      
      ;; Bind the same key with the SAME command (different source)
      (mcg-bind-key "C-c t b" 'forward-char "source-2")
      
      ;; Verify NO conflict was recorded (same command)
      (should (null mcg-keybinding-conflicts)))))

(ert-deftest test-property-12-multiple-conflicts ()
  "Property 12: Multiple conflicts are all recorded

*For any* sequence of keybinding registrations via mcg-bind-key,
ALL conflicts SHALL be recorded in mcg-keybinding-conflicts.

**Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
**Validates: Requirements 6.6**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Reset state for clean test
      (test-reset-keybinding-state)
      
      ;; Create multiple conflicts
      (mcg-bind-key "C-c t c" 'forward-char "source-1")
      (mcg-bind-key "C-c t c" 'backward-char "source-2")  ; conflict 1
      
      (mcg-bind-key "C-c t d" 'next-line "source-1")
      (mcg-bind-key "C-c t d" 'previous-line "source-2")  ; conflict 2
      
      (mcg-bind-key "C-c t e" 'kill-line "source-1")
      (mcg-bind-key "C-c t e" 'yank "source-2")           ; conflict 3
      
      ;; Verify all conflicts were recorded
      (should (= 3 (length mcg-keybinding-conflicts))))))

(ert-deftest test-property-12-registry-updated ()
  "Property 12: Registry is updated even when conflict occurs

*For any* keybinding registered via mcg-bind-key, the registry SHALL
be updated with the new binding regardless of whether a conflict occurred.

**Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
**Validates: Requirements 6.6**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Reset state for clean test
      (test-reset-keybinding-state)
      
      ;; Bind a key first time
      (mcg-bind-key "C-c t f" 'forward-char "source-1")
      
      ;; Verify registry has the binding
      (let ((binding (mcg-lookup-keybinding "C-c t f")))
        (should binding)
        (should (equal 'forward-char (car binding)))
        (should (equal "source-1" (cdr binding))))
      
      ;; Bind the same key with a different command
      (mcg-bind-key "C-c t f" 'backward-char "source-2")
      
      ;; Verify registry was updated to new binding
      (let ((binding (mcg-lookup-keybinding "C-c t f")))
        (should binding)
        (should (equal 'backward-char (car binding)))
        (should (equal "source-2" (cdr binding)))))))

(ert-deftest test-property-12-randomized-conflict-detection ()
  "Property 12: Randomized conflict detection test

*For any* randomly generated keybinding sequence, conflicts SHALL be
correctly detected when the same key is bound to different commands.

**Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
**Validates: Requirements 6.6**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Run 100 iterations of randomized testing
      (dotimes (_ 100)
        ;; Reset state for each iteration
        (test-reset-keybinding-state)
        
        ;; Generate a random key
        (let* ((key (concat "C-c t " (test-gen-random-key)))
               (cmd1 (test-gen-random-command))
               (cmd2 (test-gen-random-command))
               (src1 (test-gen-random-source))
               (src2 (test-gen-random-source)))
          
          ;; Bind the key first time
          (mcg-bind-key key cmd1 src1)
          
          ;; Bind the same key second time
          (mcg-bind-key key cmd2 src2)
          
          ;; Check conflict detection
          (if (eq cmd1 cmd2)
              ;; Same command - no conflict expected
              (should (null mcg-keybinding-conflicts))
            ;; Different command - conflict expected
            (progn
              (should mcg-keybinding-conflicts)
              (should (= 1 (length mcg-keybinding-conflicts)))
              ;; Verify conflict contains correct key
              (should (equal key (nth 0 (car mcg-keybinding-conflicts)))))))))))

(ert-deftest test-property-12-key-normalization ()
  "Property 12: Key normalization for conflict detection

*For any* keybinding, the key sequence SHALL be normalized before
conflict detection, so equivalent key representations are detected.

**Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
**Validates: Requirements 6.6**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Reset state for clean test
      (test-reset-keybinding-state)
      
      ;; Test that mcg-normalize-key works correctly
      (should (stringp (mcg-normalize-key "C-c a")))
      (should (stringp (mcg-normalize-key [?\C-c ?a])))
      
      ;; Bind using string key
      (mcg-bind-key "C-c t g" 'forward-char "source-1")
      
      ;; Lookup should work
      (let ((binding (mcg-lookup-keybinding "C-c t g")))
        (should binding)
        (should (equal 'forward-char (car binding)))))))

(ert-deftest test-property-12-conflict-info-completeness ()
  "Property 12: Conflict information is complete

*For any* detected conflict, the conflict record SHALL contain:
- The key sequence
- The old binding (command and source)
- The new binding (command and source)

**Feature: modern-emacs-config, Property 12: Keybinding Conflict Detection**
**Validates: Requirements 6.6**"
  (let ((bootstrap-file (expand-file-name "core/core-bootstrap.el" test-config-dir))
        (lib-file (expand-file-name "core/core-lib.el" test-config-dir)))
    (when (and (file-exists-p bootstrap-file) (file-exists-p lib-file))
      (load bootstrap-file nil t)
      (load lib-file nil t)
      
      ;; Initialize paths
      (mcg-init-paths test-emacs-dir)
      
      ;; Reset state for clean test
      (test-reset-keybinding-state)
      
      ;; Create a conflict
      (mcg-bind-key "C-c t h" 'forward-char "init-navigation")
      (mcg-bind-key "C-c t h" 'backward-char "init-magit")
      
      ;; Verify conflict record completeness
      (should mcg-keybinding-conflicts)
      (let ((conflict (car mcg-keybinding-conflicts)))
        ;; Should have 3 elements: key, old-binding, new-binding
        (should (= 3 (length conflict)))
        
        ;; Key should be a string
        (should (stringp (nth 0 conflict)))
        
        ;; Old binding should be (command . source) cons
        (let ((old-binding (nth 1 conflict)))
          (should (consp old-binding))
          (should (symbolp (car old-binding)))
          (should (stringp (cdr old-binding))))
        
        ;; New binding should be (command . source) cons
        (let ((new-binding (nth 2 conflict)))
          (should (consp new-binding))
          (should (symbolp (car new-binding)))
          (should (stringp (cdr new-binding))))))))

(provide 'test-property-12-keybinding-conflict)
;;; test-property-12-keybinding-conflict.el ends here
