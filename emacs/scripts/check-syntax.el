;;; check-syntax.el --- Check Emacs Lisp syntax -*- lexical-binding: t; -*-

(defun check-file-syntax (file)
  "Check if FILE has balanced parentheses."
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (forward-sexp))
        (message "✓ Syntax OK: %s" file)
        t)
    (error
     (message "✗ Syntax Error in %s: %s" file err)
     nil)))

;; Check the file passed as argument
(when command-line-args-left
  (let ((file (car command-line-args-left)))
    (if (check-file-syntax file)
        (kill-emacs 0)
      (kill-emacs 1))))

;;; check-syntax.el ends here
