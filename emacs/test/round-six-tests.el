;;; round-six-tests.el --- Module declaration and scheduling regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'core-modules)

(defmacro mcg-test-round-six-registry (&rest body)
  "Run BODY with an isolated declaration registry."
  (declare (indent 0))
  `(let ((mcg-modules-alist nil)
         (mcg-disabled-modules nil)
         (mcg-module-load-order nil)
         (mcg-module-metadata-alist nil))
     ,@body))

(ert-deftest mcg-round-six-repeated-module-uses-last-state-and-first-position ()
  (mcg-test-round-six-registry
    (mcg! :tools (alpha :load immediate) beta -alpha
          :editor alpha
          :tools (alpha :load idle))
    (should (equal mcg-module-load-order
                   '((:tools . alpha) (:tools . beta) (:editor . alpha))))
    (should (equal (cdr (assq :tools mcg-modules-alist)) '(alpha beta)))
    (should-not (mcg-module-disabled-p :tools 'alpha))
    (should (eq (mcg-module-load-strategy :tools 'alpha) 'idle))
    (should (= 3 (length mcg-module-metadata-alist)))))

(ert-deftest mcg-round-six-final-disabled-declaration-removes-enabled-metadata ()
  (mcg-test-round-six-registry
    (mcg! :tools (alpha :load idle) beta -alpha -alpha)
    (should-not (mcg-module-enabled-p :tools 'alpha))
    (should (equal mcg-disabled-modules '((:tools alpha))))
    (should (equal mcg-module-load-order '((:tools . beta))))
    (should-not (mcg-module-metadata :tools 'alpha))))

(ert-deftest mcg-round-six-deferred-setup-respects-strategy-and-category ()
  (mcg-test-round-six-registry
    (mcg! :test eager (idle :load idle) (lazy :load deferred) -disabled
          :other (lazy :load deferred))
    (let ((mcg-module-triggers
           '(((:test . eager) . ("eager" . fundamental-mode))
             ((:test . idle) . ("idle" . fundamental-mode))
             ((:test . lazy) . ("lazy" . fundamental-mode))
             ((:other . lazy) . ("other" . fundamental-mode))))
          calls)
      (cl-letf (((symbol-function 'mcg-defer-module)
                 (lambda (category module &optional _trigger)
                   (push (cons category module) calls))))
        (mcg-setup-deferred-loading '(:test))
        (should (equal calls '((:test . lazy))))))))

(ert-deftest mcg-round-six-deferred-without-trigger-reports-module ()
  (mcg-test-round-six-registry
    (mcg! :test (missing :load deferred))
    (let ((mcg-module-triggers nil))
      (let ((err (should-error (mcg-setup-deferred-loading) :type 'user-error)))
        (should (string-match-p ":test/missing" (error-message-string err)))))))

(ert-deftest mcg-round-six-idle-setup-replaces-previous-timer ()
  (let ((mcg-idle-load-modules '((:test . alpha)))
        (mcg-idle-load-delay 3600)
        (mcg-idle-load-timer nil)
        first second)
    (unwind-protect
        (progn
          (mcg-setup-idle-loading)
          (setq first mcg-idle-load-timer)
          (mcg-setup-idle-loading)
          (setq second mcg-idle-load-timer)
          (should (memq second timer-idle-list))
          (should-not (memq first timer-idle-list)))
      (dolist (timer (list first second mcg-idle-load-timer))
        (when (timerp timer) (cancel-timer timer))))))

(ert-deftest mcg-round-six-empty-idle-queue-cancels-scheduled-load ()
  (let ((mcg-idle-load-modules '((:test . alpha)))
        (mcg-idle-load-delay 3600)
        (mcg-idle-load-timer nil)
        timer)
    (unwind-protect
        (progn
          (mcg-setup-idle-loading)
          (setq timer mcg-idle-load-timer mcg-idle-load-modules nil)
          (mcg-setup-idle-loading)
          (should-not mcg-idle-load-timer)
          (should-not (memq timer timer-idle-list)))
      (when (timerp timer) (cancel-timer timer)))))

(ert-deftest mcg-round-six-idle-drains-once-and-can-requeue-failures ()
  (let ((mcg-idle-load-modules '((:test . alpha) (:test . beta)))
        (mcg-idle-loaded-modules nil)
        (mcg-loaded-modules nil)
        (mcg-idle-load-delay 3600)
        (mcg-idle-load-timer nil)
        (fail t)
        calls)
    (unwind-protect
        (cl-letf (((symbol-function 'mcg-load-module)
                   (lambda (_category module)
                     (push module calls)
                     (not (and fail (eq module 'alpha))))))
          (mcg--idle-load-next)
          (should (timerp mcg-idle-load-timer))
          (mcg--idle-load-next)
          (should-not mcg-idle-load-timer)
          (should (equal mcg-idle-loaded-modules '((:test . beta))))
          (setq fail nil)
          (mcg-add-idle-load-module :test 'alpha)
          (mcg--idle-load-next)
          (should (equal (reverse calls) '(alpha beta alpha)))
          (should (member '(:test . alpha) mcg-idle-loaded-modules)))
      (when (timerp mcg-idle-load-timer) (cancel-timer mcg-idle-load-timer)))))

(ert-deftest mcg-round-six-default-declarations-have-files-and-deferred-triggers ()
  (mcg-test-round-six-registry
    (with-temp-buffer
      (insert-file-contents (expand-file-name "site-lisp/config/init.el" mcg-test-root))
      (goto-char (point-min))
      (let (declaration)
        (condition-case nil
            (while (not declaration)
              (let ((form (read (current-buffer))))
                (when (eq (car-safe form) 'mcg!) (setq declaration form))))
          (end-of-file nil))
        (should declaration)
        (eval declaration t)))
    (let ((mcg-modules-dir (expand-file-name "site-lisp/config/modules" mcg-test-root)))
      (dolist (key mcg-module-load-order)
        (should (mcg-module-exists-p (car key) (cdr key)))
        (when (eq (mcg-module-load-strategy (car key) (cdr key)) 'deferred)
          (should (assoc key mcg-module-triggers)))))))

;;; round-six-tests.el ends here
