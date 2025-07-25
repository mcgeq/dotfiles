;;; init-embark.el -- Config for embark -*- lexical-binding: t; -*-

;; Filename: init.el
;; Description: Config for embark
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:21>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for embark
;;

;;; Installation:
;;
;; Put init-embark.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-embark)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-aider RET
;;

;;; Change log:
;;

;;; Require:
(require 'dired)
(require 'embark)
(require 'embark-consult)

;; consult
(add-hook 'completion-list-mode-hook 'consult-preview-at-point-mode)
(setq ;register-preview-delay 0.5
 register-preview-function #'consult-register-format)

(advice-add #'register-preview :override #'consult-register-window)

;; Use consult to select xref locations with preview
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; custom function
(defun mcg/consult-find-org-headings (&optional match)
  "MATCH find headngs in all org files."
    (interactive)
    (consult-org-heading match (directory-files org-directory t "^[0-9]\\{8\\}.+\\.org$")))

;; Use `consult-ripgrep' instead of `consult-line' in large buffers
(defun consult-line-symbol-at-point ()
   "Consult line the symbol where the point is."
    (interactive)
    (consult-line (thing-at-point 'symbol)))

;; keymap
(lazy-load-set-keys
 '(
   ("C-c C-j" . consult-outline)
   )
 prog-mode-map)


(lazy-load-set-keys
 '(
   ("C-r" . consult-history)
   )
 minibuffer-local-map)

;; embark

(setq embark-quit-after-action nil)
(setq prefix-help-command #'embark-prefix-help-command)
(setq embark-indicators '(embark-minimal-indicator
                          embark-highlight-indicator
                          embark-isearch-highlight-indicator))
(setq embark-cycle-key ".")
(setq embark-help-key "?")

(setq embark-candidate-collectors
      (cl-substitute 'embark-sorted-minibuffer-candidates
                     'embark-minibuffer-candidates
                     embark-candidate-collectors))
(defun dired-open-externally (&optional arg)
  "Open ARG marked or current file in operating system's default application."
  (interactive "P")
  (dired-map-over-marks (embark-open-externally (dired-get-filename)) arg))

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

(add-hook 'embark-collect-mode #'consult-preview-at-point-mode)

(provide 'init-embark)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-embark.el ends here
