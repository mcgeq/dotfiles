;;; round-three-tests.el --- Map-scoped key registry regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'core-lib)

(defmacro mcg-test-round-three-keys (&rest body)
  "Run BODY with isolated maps and a fresh key registry."
  (declare (indent 0))
  `(let ((global-map (make-sparse-keymap))
         (local-a (make-sparse-keymap))
         (local-b (make-sparse-keymap))
         (mcg-keybinding-registry nil)
         (mcg-keybinding-conflicts nil)
         (mcg-keybinding-conflict-policy 'reject))
     ,@body))

(ert-deftest mcg-round-three-same-key-in-distinct-maps-is-independent ()
  (mcg-test-round-three-keys
    (should (equal local-a local-b))
    (should-not (eq local-a local-b))
    (should (mcg-bind-key "C-c t" 'ignore "a" local-a))
    (should (mcg-bind-key "C-c t" 'forward-char "b" local-b))
    (should (equal (mcg-lookup-keybinding "C-c t" local-a) '(ignore . "a")))
    (should (equal (mcg-lookup-keybinding "C-c t" local-b) '(forward-char . "b")))
    (should-not (mcg-lookup-keybinding "C-c t"))
    (should-not mcg-keybinding-conflicts)))

(ert-deftest mcg-round-three-registry-survives-keymap-mutation ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t" 'ignore "local" local-a)
    (define-key local-a (kbd "C-c x") 'forward-char)
    (should (equal (mcg-lookup-keybinding "C-c t" local-a) '(ignore . "local")))
    (should-not (mcg-lookup-keybinding "C-c t" local-b))))

(ert-deftest mcg-round-three-prefix-conflicts-stay-in-their-map ()
  (mcg-test-round-three-keys
    (should (mcg-bind-key "C-c t" 'ignore "a" local-a))
    (should (mcg-bind-key "C-c t x" 'forward-char "b" local-b))
    (should-not (mcg-bind-key "C-c t x" 'forward-char "a" local-a))
    (should-not (mcg-bind-key "C-c t" 'ignore "b" local-b))
    (should (= 2 (length mcg-keybinding-conflicts)))
    (should (eq (nth 3 (car mcg-keybinding-conflicts)) local-b))))

(ert-deftest mcg-round-three-vector-and-string-keys-share-a-record ()
  (mcg-test-round-three-keys
    (should (mcg-bind-key [f8] 'ignore "vector" local-a))
    (should (eq (lookup-key local-a [f8]) 'ignore))
    (should (equal (mcg-lookup-keybinding "<f8>" local-a) '(ignore . "vector")))
    (should-not (mcg-bind-key "<f8>" 'forward-char "string" local-a))
    (mcg-unbind-key [f8] local-a)
    (should-not (mcg-lookup-keybinding "<f8>" local-a))))

(ert-deftest mcg-round-three-equivalent-string-spelling-detects-conflict ()
  (mcg-test-round-three-keys
    (should (mcg-bind-key "C-c   t" 'ignore "first"))
    (should-not (mcg-bind-key "C-c t" 'forward-char "second"))
    (should (equal (mcg-list-keybindings) '(("C-c t" ignore "first"))))))

(ert-deftest mcg-round-three-failed-binding-does-not-register ()
  (mcg-test-round-three-keys
    (define-key local-a (kbd "C-c t") 'ignore)
    (should-error (mcg-bind-key "C-c t x" 'forward-char "failed" local-a))
    (should-not (mcg-lookup-keybinding "C-c t x" local-a))
    (should-not (mcg-list-keybindings local-a))))

(ert-deftest mcg-round-three-failed-rebind-preserves-previous-record ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t" 'ignore "before" local-a)
    (let ((mcg-keybinding-conflict-policy 'overwrite))
      (cl-letf (((symbol-function 'define-key) (lambda (&rest _) (error "fixture failure"))))
        (should-error (mcg-bind-key "C-c t" 'forward-char "after" local-a))))
    (should (equal (mcg-lookup-keybinding "C-c t" local-a) '(ignore . "before")))
    (should (eq (lookup-key local-a (kbd "C-c t")) 'ignore))))

(ert-deftest mcg-round-three-failed-unbind-preserves-record ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t" 'ignore "local" local-a)
    (cl-letf (((symbol-function 'define-key) (lambda (&rest _) (error "fixture failure"))))
      (should-error (mcg-unbind-key "C-c t" local-a)))
    (should (equal (mcg-lookup-keybinding "C-c t" local-a) '(ignore . "local")))))

(ert-deftest mcg-round-three-unbind-affects-only-target-map ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t" 'ignore "global")
    (mcg-bind-key "C-c t" 'ignore "local" local-a)
    (mcg-unbind-key "C-c t" local-a)
    (should (equal (mcg-lookup-keybinding "C-c t") '(ignore . "global")))
    (should-not (mcg-lookup-keybinding "C-c t" local-a))
    (should (eq (lookup-key global-map (kbd "C-c t")) 'ignore))))

(ert-deftest mcg-round-three-prefix-unbind-removes-descendant-records ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t x" 'ignore "child" local-a)
    (mcg-unbind-key "C-c t" local-a)
    (should-not (mcg-list-keybindings local-a))))

(ert-deftest mcg-round-three-overwrite-prefix-removes-obsolete-descendants ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t x" 'ignore "child" local-a)
    (let ((mcg-keybinding-conflict-policy 'overwrite))
      (should (mcg-bind-key "C-c t" 'forward-char "parent" local-a)))
    (should (equal (mcg-list-keybindings local-a) '(("C-c t" forward-char "parent"))))))

(ert-deftest mcg-round-three-global-list-keeps-three-field-rows ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t" 'ignore "global")
    (mcg-bind-key "C-c t" 'ignore "local" local-a)
    (should (equal (mcg-list-keybindings) '(("C-c t" ignore "global"))))
    (should (equal (mcg-list-keybindings local-a) '(("C-c t" ignore "local"))))))

(ert-deftest mcg-round-three-reports-identify-keymaps-and-prefix-conflicts ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t" 'ignore "global-source")
    (mcg-bind-key "C-c t x" 'ignore "local-source" local-a)
    (mcg-bind-key "C-c t" 'forward-char "blocked-source" local-a)
    (cl-letf (((symbol-function 'display-buffer) #'ignore))
      (mcg-show-keybindings)
      (mcg-show-keybinding-conflicts))
    (with-current-buffer "*MCG Keybindings*"
      (should (string-match-p "global-map" (buffer-string)))
      (should (string-match-p "keymap-1" (buffer-string)))
      (should (string-match-p "local-source" (buffer-string))))
    (with-current-buffer "*MCG Keybinding Conflicts*"
      (should (string-match-p "keymap-1" (buffer-string)))
      (should (string-match-p "local-source" (buffer-string)))
      (should (string-match-p "would-block-longer" (buffer-string))))))

(ert-deftest mcg-round-three-cache-detects-new-container-child-with-same-mtime ()
  (mcg-test-round-two-extensions
    (mcg-list-extensions)
    (let* ((mtime (file-attribute-modification-time (file-attributes root)))
           (package (mcg-test-round-two-package root "new")))
      (set-file-times root mtime)
      (should (equal (mcg-extension-path 'new) package)))))

(ert-deftest mcg-round-three-cache-detects-new-lisp-dir-with-same-mtime ()
  (mcg-test-round-two-extensions
    (let ((package (mcg-test-round-two-package root "package")))
      (mcg-list-extensions)
      (let* ((mtime (file-attribute-modification-time (file-attributes package)))
             (lisp (mcg-test-round-two-package root "package/lisp")))
        (set-file-times package mtime)
        (should (equal (mcg-extension-path 'package) lisp))))))

(ert-deftest mcg-round-three-legacy-flat-registry-imports-global-bindings ()
  (mcg-test-round-three-keys
    (setq mcg-keybinding-registry (make-hash-table :test 'equal))
    (puthash "C-c   t" '(ignore . "legacy") mcg-keybinding-registry)
    (mcg-keybinding-init)
    (should (eq (hash-table-test mcg-keybinding-registry) 'eq))
    (should (equal (mcg-lookup-keybinding "C-c t") '(ignore . "legacy")))
    (should-not (mcg-lookup-keybinding "C-c t" local-a))))

(ert-deftest mcg-round-three-overwrite-exact-key-keeps-other-map ()
  (mcg-test-round-three-keys
    (mcg-bind-key "C-c t" 'ignore "a" local-a)
    (mcg-bind-key "C-c t" 'ignore "b" local-b)
    (let ((mcg-keybinding-conflict-policy 'overwrite))
      (should (mcg-bind-key "C-c t" 'forward-char "replacement" local-a)))
    (should (equal (mcg-lookup-keybinding "C-c t" local-a) '(forward-char . "replacement")))
    (should (equal (mcg-lookup-keybinding "C-c t" local-b) '(ignore . "b")))
    (should (eq (nth 3 (car mcg-keybinding-conflicts)) local-a))))

(ert-deftest mcg-round-three-prefix-map-children-survive-idempotent-bind ()
  (mcg-test-round-three-keys
    (let ((prefix (make-sparse-keymap)))
      (mcg-bind-key "C-c t" prefix "prefix" local-a)
      (mcg-bind-key "C-c t x" 'ignore "child" local-a)
      (mcg-bind-key "C-c t" prefix "prefix" local-a)
      (should (equal (mcg-lookup-keybinding "C-c t x" local-a) '(ignore . "child")))
      (should-not mcg-keybinding-conflicts))))

(provide 'round-three-tests)
