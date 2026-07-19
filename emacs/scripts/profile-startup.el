;;; profile-startup.el --- Batch startup profiler for MCG Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Run from the emacs/ directory with:
;;
;;   emacs -Q --batch -l scripts/profile-startup.el
;;
;; or:
;;
;;   make profile-startup
;;
;; This measures generated configuration load time in batch mode.  It is not a
;; GUI first-frame benchmark, but it gives a stable module-level signal for
;; startup regressions.

;;; Code:

(defvar mcg-profile-startup-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Root directory of the Emacs configuration being profiled.")

(defvar mcg-profile-startup-key-modules
  '(":ui/ef-themes"
    ":ui/font"
    ":ui/nerd-icons"
    ":ui/pulsar"
    ":editor/defaults"
    ":completion/consult"
    ":completion/embark"
    ":enhance/recentf"
    "private/config")
  "Startup entries to highlight after the full sorted report.")

(defun mcg-profile-startup-load (relative-path)
  "Load RELATIVE-PATH below `mcg-profile-startup-root'."
  (let ((file (expand-file-name relative-path mcg-profile-startup-root)))
    (unless (file-exists-p file)
      (error "Startup profile file not found: %s" file))
    (load file nil t)))

(defun mcg-profile-startup--ignore-output (&rest _args)
  "Ignore noisy output while profiling startup."
  nil)

(defun mcg-profile-startup-quiet-output ()
  "Suppress third-party messages and warnings in this batch profiler."
  (dolist (function '(message display-warning byte-compile-warn))
    (advice-add function :override #'mcg-profile-startup--ignore-output)))

(defun mcg-profile-startup-format-entry (entry)
  "Return a display string for load-time ENTRY."
  (format "%-28s %8.1fms" (car entry) (cdr entry)))

(defun mcg-profile-startup-sorted-load-times ()
  "Return `mcg-load-times' sorted by descending elapsed time."
  (sort (copy-sequence mcg-load-times)
        (lambda (left right) (> (cdr left) (cdr right)))))

(defun mcg-profile-startup-print-report (elapsed-ms)
  "Print startup profile report with total ELAPSED-MS."
  (let ((sorted (mcg-profile-startup-sorted-load-times)))
    (princ "MCG Emacs startup profile\n")
    (princ (format "Root: %s\n" mcg-profile-startup-root))
    (princ (format "Generated config load: %.1fms\n" elapsed-ms))
    (princ (format "Immediate modules recorded: %d\n\n" (length sorted)))
    (princ "Module load times:\n")
    (dolist (entry sorted)
      (princ (format "  %s\n" (mcg-profile-startup-format-entry entry))))
    (princ "\nKey startup entries:\n")
    (dolist (name mcg-profile-startup-key-modules)
      (let ((entry (assoc name mcg-load-times)))
        (princ
         (format "  %-28s %s\n"
                 name
                 (if entry
                     (format "%8.1fms" (cdr entry))
                   "not recorded")))))
    (when (boundp 'mcg-idle-load-modules)
      (princ (format "\nIdle queue pending: %d\n" (length mcg-idle-load-modules)))
      (dolist (entry mcg-idle-load-modules)
        (princ (format "  %S\n" entry))))
    (when (and (boundp 'mcg-module-load-errors)
               mcg-module-load-errors)
      (princ (format "\nModule load errors: %d\n" (length mcg-module-load-errors)))
      (dolist (entry mcg-module-load-errors)
        (princ (format "  %S: %s\n" (car entry) (plist-get (cdr entry) :message)))))))

(defun mcg-profile-startup-run ()
  "Profile generated MCG Emacs configuration startup in batch mode."
  (let ((default-directory mcg-profile-startup-root)
        (user-emacs-directory (file-name-as-directory mcg-profile-startup-root))
        (user-init-file (expand-file-name "init.el" mcg-profile-startup-root))
        (start-time (current-time)))
    (setq mcg-profile-mode t
          mcg-debug-mode nil
          inhibit-message t
          warning-minimum-level :error)
    (when (boundp 'native-comp-async-report-warnings-errors)
      (setq native-comp-async-report-warnings-errors nil))
    (when (boundp 'native-comp-jit-compilation)
      (setq native-comp-jit-compilation nil))
    (when (boundp 'native-comp-enable-subr-trampolines)
      (setq native-comp-enable-subr-trampolines nil))
    (when (boundp 'byte-compile-warnings)
      (setq byte-compile-warnings nil))
    (mcg-profile-startup-quiet-output)
    (mcg-profile-startup-load "site-lisp/config/early-init.el")
    (mcg-profile-startup-load "site-lisp/config/init.el")
    (mcg-profile-startup-print-report
     (* 1000.0 (float-time (time-subtract (current-time) start-time))))))

(mcg-profile-startup-run)

;;; profile-startup.el ends here
