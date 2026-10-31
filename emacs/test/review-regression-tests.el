;;; review-regression-tests.el --- Cross-round review regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defvar org-babel-default-header-args:jupyter-python)

(ert-deftest mcg-review-jupyter-header-changes-are-applied-before-caching ()
  (require 'init-org-babel)
  (let ((mcg-org-babel--configured-options nil)
        (mcg-org-babel-enable-jupyter t)
        (mcg-org-babel-enable-zig nil)
        (mcg-org-babel-src-block-languages nil)
        (mcg-org-babel-jupyter-default-header-args '((:session . "new")))
        (org-babel-default-header-args:jupyter-python '((:session . "old"))))
    (cl-letf (((symbol-function 'mcg-org-babel--jupyter-available-p) (lambda () t))
              ((symbol-function 'org-babel-do-load-languages) #'ignore))
      (mcg-org-babel-mode-setup)
      (should (equal org-babel-default-header-args:jupyter-python
                     '((:session . "new"))))
      (setq mcg-org-babel-jupyter-default-header-args '((:session . "changed")))
      (mcg-org-babel-mode-setup)
      (should (equal org-babel-default-header-args:jupyter-python
                     '((:session . "changed")))))))

(ert-deftest mcg-review-jupyter-becoming-available-installs-defaults-and-candidate ()
  (require 'init-org-babel)
  (let ((mcg-org-babel--configured-options nil)
        (mcg-org-babel-enable-jupyter t)
        (mcg-org-babel-enable-zig nil)
        (mcg-org-babel--jupyter-missing-warned nil)
        (mcg-org-babel-src-block-languages '("python"))
        (mcg-org-babel-jupyter-default-header-args '((:session . "late")))
        (org-babel-default-header-args:jupyter-python nil)
        available)
    (cl-letf (((symbol-function 'mcg-org-babel--jupyter-available-p)
               (lambda () available))
              ((symbol-function 'org-babel-do-load-languages) #'ignore))
      (mcg-org-babel-mode-setup)
      (setq available t)
      (mcg-org-babel-mode-setup)
      (should (equal org-babel-default-header-args:jupyter-python
                     '((:session . "late"))))
      (should (member "jupyter-python" mcg-org-babel-src-block-languages)))))

(ert-deftest mcg-review-cache-detects-last-lisp-file-removal-with-same-mtime ()
  (mcg-test-round-two-extensions
    (let* ((package (mcg-test-round-two-package root "package"))
           (lisp (mcg-test-round-two-package root "package/lisp"))
           (mtime (file-attribute-modification-time (file-attributes lisp))))
      (should (equal (mcg-extension-path 'package) lisp))
      (delete-file (expand-file-name "fixture.el" lisp))
      (set-file-times lisp mtime)
      (should-not (mcg-cache-valid-p))
      (should (equal (mcg-extension-path 'package) package)))))

(defmacro mcg-test-review-partial-module (&rest body)
  "Run BODY with a module that defines two commands before failing setup."
  (declare (indent 0))
  `(let* ((root (make-temp-file "mcg-review-lazy" t))
          (mcg-modules-dir root)
          (mcg-loaded-modules nil)
          (mcg-module-load-errors nil)
          (mcg-module-error-traces nil)
          (mcg-module-load-hook nil)
          (mcg-idle-load-modules '((:review . partial)))
          (mcg-idle-loaded-modules nil)
          (mcg-idle-load-timer nil)
          (mcg-lazy-module-commands nil)
          (module-file (expand-file-name "review/init-partial.el" root)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory module-file) t)
           (write-region
            "(defun mcg-review-first () (interactive) 'first)\n(defun mcg-review-second () (interactive) 'second)\n(error \"partial failure\")\n"
            nil module-file)
           (mcg-define-lazy-module-command :review 'partial 'mcg-review-first)
           (mcg-define-lazy-module-command :review 'partial 'mcg-review-second)
           ,@body)
       (dolist (command '(mcg-review-first mcg-review-second))
         (fmakunbound command)
         (put command 'mcg-lazy-module-stub nil))
       (when (timerp mcg-idle-load-timer) (cancel-timer mcg-idle-load-timer))
       (delete-directory root t))))

(ert-deftest mcg-review-background-failure-keeps-all-command-entries-retryable ()
  (mcg-test-review-partial-module
    (mcg--idle-load-next)
    (should-not mcg-loaded-modules)
    (should mcg-module-load-errors)
    (should-error (call-interactively 'mcg-review-second) :type 'user-error)
    (write-region
     "(defun mcg-review-first () (interactive) 'ready-first)\n(defun mcg-review-second () (interactive) 'ready-second)\n"
     nil module-file)
    (should (eq (call-interactively 'mcg-review-second) 'ready-second))
    (should (mcg-module-loaded-p :review 'partial))
    (should-not mcg-module-load-errors)))

(ert-deftest mcg-review-lazy-failure-keeps-sibling-command-entries-retryable ()
  (mcg-test-review-partial-module
    (should-error (call-interactively 'mcg-review-first) :type 'user-error)
    (should-error (call-interactively 'mcg-review-second) :type 'user-error)
    (write-region
     "(defun mcg-review-first () (interactive) 'ready-first)\n(defun mcg-review-second () (interactive) 'ready-second)\n"
     nil module-file)
    (should (eq (call-interactively 'mcg-review-first) 'ready-first))
    (should (eq (call-interactively 'mcg-review-second) 'ready-second))
    (should-not mcg-module-load-errors)))

(ert-deftest mcg-review-real-command-workflows-in-fresh-process ()
  (with-temp-buffer
    (let ((status (call-process
                   (expand-file-name invocation-name invocation-directory)
                   nil t nil "-Q" "--batch" "-l"
                   (expand-file-name "test/cumulative-workflow-smoke.el" mcg-test-root))))
      (ert-info ((buffer-string)) (should (zerop status))))))

;;; review-regression-tests.el ends here
