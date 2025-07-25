;;; init-vertico.el -- Config for Vertico -*- lexical-binding: t; -*-

;; Filename: init-vertico.el
;; Description: Config for Vertico
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:24>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Vertico
;;

;;; Installation:
;;
;; Put init-vertico.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-vertico)
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

(require 'crm)
(require 'vertico)
(require 'vertico-directory)
(require 'orderless)
(require 'pinyinlib)

;; pinyinlib
;; orderless
;; Fuzzy find orders
(setq completion-styles '(orderless partial-completion basic)
      orderless-component-separator "[ &]" ;; Company
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; make completion support pinyin
;; refer to https://emacs-china.org/t/vertico/17913/2
(defun completion-regex-pinyin (str)
  (orderless-regexp (pinyinlib-build-regexp-string str)))
(add-to-list 'orderless-matching-styles 'completion-regex-pinyin)

;; vertico
(add-hook 'after-init-hook #'vertico-mode)
(setq vertico-count 15)                       ;; Number of candidates to display
(setq vertico-resize nil)
;; Optionally enable cycling for `vertico-next' and `vertico-previous'
(setq vertico-cycle t)
;; Go from last to first candidate and first to last (cycle)?
(setq vertico-mode t)

;; And prompt indicator to 'completing-read-multiple.'
;; We display [CRM<separator>], e.g:, [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  "Crm indicator.  ARGS."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; vertico
(lazy-load-set-keys
 '(
   ("<tab>"    .   vertico-insert)  ; Choose selected candidate
   ("<escape>" .   vertico-exit)    ; Close minibuffer
   ("DEL"      .   vertico-directory-delete-char)
   ("C-M-n"    .   vertico-next-group)
   ("C-M-p"    .   vertico-previous-group)
   )
 vertico-map)

(provide 'init-vertico)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vertico.el ends here
