;;; round-one-tests.el --- Startup and tool selection regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'core-modules)

(defvar mcg-after-init-hook nil)
(defvar mcg-test-round-one-option nil)

(defun mcg-test-round-one-write-module (contents file)
  "Write a lexical-binding module fixture with CONTENTS to FILE."
  (write-region (concat ";;; -*- lexical-binding: t; -*-\n" contents) nil file))

(defun mcg-test-round-one-init-functions ()
  "Load entrypoint functions without starting the user's configuration."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "site-lisp/config/init.el" mcg-emacs-dir))
    (goto-char (point-min))
    (condition-case nil
        (while t
          (let ((form (read (current-buffer))))
            (when (eq (car-safe form) 'defun)
              (eval form t))))
      (end-of-file nil))))

(ert-deftest mcg-round-one-private-options-precede-feature-modules ()
  (mcg-test-round-one-init-functions)
  (let ((mcg-test-round-one-option nil)
        (mcg-after-init-hook nil)
        (emacs-startup-hook nil)
        events)
    (cl-letf (((symbol-function 'mcg-load-private-config)
               (lambda () (setq mcg-test-round-one-option 'configured)
                 (push 'options events)))
              ((symbol-function 'mcg-setup-idle-module-command-entries) #'ignore)
              ((symbol-function 'mcg-load-immediate-modules)
               (lambda ()
                 (should (eq mcg-test-round-one-option 'configured))
                 (push 'modules events)))
              ((symbol-function 'mcg-setup-idle-modules) #'ignore)
              ((symbol-function 'mcg-setup-deferred-modules) #'ignore))
      (add-hook 'mcg-after-init-hook (lambda () (push 'after-init events)))
      (mcg-init)
      (should (equal (nreverse events) '(options modules after-init))))))

(defmacro mcg-test-round-one-module (&rest body)
  "Run BODY with an isolated module registry and a temporary module file."
  (declare (indent 0))
  `(let* ((root (make-temp-file "mcg-round-one" t))
          (mcg-modules-dir root)
          (mcg-loaded-modules nil)
          (mcg-module-load-errors nil)
          (mcg-module-error-traces nil)
          (mcg-module-load-hook nil)
          (mcg-profile-mode nil)
          (module-dir (expand-file-name "test" root))
          (module-file (expand-file-name "init-round-one.el" module-dir)))
     (unwind-protect
         (progn (make-directory module-dir t) ,@body)
       (delete-directory root t))))

(ert-deftest mcg-round-one-setup-failure-is-recorded-and-can-retry ()
  (mcg-test-round-one-module
    (mcg-test-round-one-write-module
     "(mcg-simple-module \"round-one\" (error \"setup failed\"))" module-file)
    (should-not (mcg-load-module :test 'round-one))
    (should-not mcg-loaded-modules)
    (should (assoc '(:test . round-one) mcg-module-load-errors))
    (mcg-test-round-one-write-module "(mcg-simple-module \"round-one\" t)" module-file)
    (should (mcg-load-module :test 'round-one))
    (should-not mcg-module-load-errors)
    (should-not mcg-module-error-traces)))

(ert-deftest mcg-round-one-defmodule-propagates-setup-failure ()
  (mcg-test-round-one-module
    (mcg-test-round-one-write-module
     "(mcg-defmodule round-one :config (error \"setup failed\"))" module-file)
    (should-not (mcg-load-module :test 'round-one))
    (should-not mcg-loaded-modules)
    (should (assoc '(:test . round-one) mcg-module-load-errors))))

(ert-deftest mcg-round-one-hook-failure-keeps-module-state-consistent ()
  (mcg-test-round-one-module
    (mcg-test-round-one-write-module "t" module-file)
    (add-hook 'mcg-module-load-hook (lambda (&rest _) (error "hook failed")))
    (should-not (mcg-load-module :test 'round-one))
    (should-not mcg-loaded-modules)
    (setq mcg-module-load-hook nil)
    (should (mcg-load-module :test 'round-one))
    (should-not mcg-module-load-errors)))

(ert-deftest mcg-round-one-deferred-failure-remains-pending ()
  (mcg-test-round-one-module
    (let ((mcg-deferred-modules '(((:test . round-one) . ("test" . fundamental-mode)))))
      (should-not (mcg--trigger-deferred-load '(:test . round-one)))
      (should (assoc '(:test . round-one) mcg-deferred-modules))
      (mcg-test-round-one-write-module "t" module-file)
      (should (mcg--trigger-deferred-load '(:test . round-one)))
      (should-not mcg-deferred-modules))))

(ert-deftest mcg-round-one-required-package-failure-reaches-loader ()
  (mcg-test-round-one-module
    (let ((mcg-modules-dir (expand-file-name "site-lisp/config/modules" mcg-emacs-dir)))
      (cl-letf (((symbol-function 'mcg-require-extension)
                 (lambda (_name &optional noerror)
                   (unless noerror (error "Required package missing")))))
        (should-not (mcg-load-module :completion 'marginalia))
        (should (assoc '(:completion . marginalia) mcg-module-load-errors))))))

(ert-deftest mcg-round-one-missing-lsp-package-is-not-marked-loaded ()
  (mcg-test-round-one-module
    (let ((mcg-modules-dir (expand-file-name "site-lisp/config/modules" mcg-emacs-dir))
          (mcg-emacs-dir root))
      (cl-letf (((symbol-function 'mcg-extension-path) (lambda (_) nil)))
        (should-not (mcg-load-module :tools 'lsp-bridge))
        (should-not mcg-loaded-modules)
        (should (assoc '(:tools . lsp-bridge) mcg-module-load-errors))))))

(ert-deftest mcg-round-one-optional-package-retains-fallback ()
  (mcg-test-round-one-module
    (mcg-test-round-one-write-module
     "(mcg-require-extension \"round-one-optional\" t)" module-file)
    (cl-letf (((symbol-function 'mcg-extension-path) (lambda (_) nil))
              ((symbol-function 'mcg-extension-direct-path) (lambda (_) nil)))
      (should (mcg-load-module :test 'round-one))
      (should-not mcg-module-load-errors))))

(ert-deftest mcg-round-one-tool-detection-does-not-load-language-modes ()
  (let ((script (make-temp-file "mcg-round-one-tools" nil ".el"))
        (root (make-temp-file "mcg-round-one-tool-root" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "uv.lock" root))
          (write-region
           (concat ";;; -*- lexical-binding: t; -*-\n(require 'core-dev-tools)\n"
                   "(when (or (featurep 'init-python) (featurep 'init-zig) "
                   "(featurep 'init-web) (featurep 'init-format)) (error \"Eager mode load\"))\n"
                   "(unless (and (fboundp 'mcg-python-format-candidates) "
                   "(fboundp 'mcg-format--web-external-candidates) "
                   "(fboundp 'mcg-zig-build-command)) (error \"Missing tools\"))\n"
                   (format
                    "(cl-letf (((symbol-function 'executable-find) (lambda (_) \"uv\"))) (unless (equal (mcg-python--pytest-command %S) \"uv run pytest\") (error \"Wrong runner before mode load\")))\n"
                    root))
           nil script)
          (with-temp-buffer
            (should (= 0 (call-process
                          (expand-file-name invocation-name invocation-directory)
                          nil t nil "-Q" "--batch" "-L"
                          (expand-file-name "site-lisp/config/core" mcg-emacs-dir)
                          "-l" script)))))
      (delete-file script)
      (delete-directory root t))))

(ert-deftest mcg-round-one-compile-and-doctor-agree-before-python-loads ()
  (require 'init-compile)
  (require 'core-doctor)
  (let ((root (make-temp-file "mcg-round-one-python" t)))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name "uv.lock" root))
          (write-region "[project]\n" nil (expand-file-name "pyproject.toml" root))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (tool) (and (equal tool "uv") "/bin/uv"))))
            (should (equal (mcg-compile--python-command root 'test) "uv run pytest"))
            (should (equal (mcg-doctor--python-pytest-command root "uv")
                           (mcg-compile--python-command root 'test)))))
      (delete-directory root t))))

;;; round-one-tests.el ends here
