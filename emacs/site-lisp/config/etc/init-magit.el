;;; init-magit.el -- Config for Magit -*- lexical-binding: t; -*-

;; Filename: init-magit.el
;; Description: Config for Magit
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:15>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Magit
;;

;;; Installation:
;;
;; Put init-magit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-magit)
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
(require 'magit)

;;; Code:
(add-hook 'git-commit-mode-hook #'flyspell-mode)
(setq magit-diff-refine-hunk t)
(setq magit-ediff-dwim-show-on-hunks t)

;; 鑷����畾涔塵agit鑿滃崟
(transient-define-prefix mcge-magit-menu ()
  "My custom Magit menu."
  [[
    ("t" "Status" magit-status)]
   [
    ("b" "Branch" mcge-magit-branch-menu)]
   [
    ("l" "Logs" mcge-magit-log-menu)]
   [
    ("d" "Diff" mcge-magit-diff-menu)]
   [
    ("s" "Submodule" mcge-magit-submodule-menu)]
   [
    ("r" "Remote" mcge-magit-remote-menu)]
   [
    ("f" "Files" mcge-magit-file-menu)]
   ])

(transient-define-prefix mcge-magit-branch-menu ()
  "Branch Command"
  [["Branch Operations"
    ("c" "Create       Branch" magit-branch-create)
    ("C" "Checkout     Branch" magit-branch-checkout)
    ("n" "New Checkout Branch" magit-branch-and-checkout)
    ("m" "Merge        Branch" magit-merge)
    ("d" "Delete       Branch" magit-branch-delete)
    ("r" "Rename       Branch" magit-branch-rename)
    ("R" "Reset        Branch" magit-branch-reset)]]
  )

(transient-define-prefix mcge-magit-submodule-menu ()
  "Submodule Command."
  [["Submodules"
    ("a" "Add    Submodule" magit-submodule-add)
    ("r" "Remove Submodule" magit-submodule-remove)
    ("u" "Update Submodule" magit-submodule-update)]]
  )

(transient-define-prefix mcge-magit-remote-menu ()
  "Remote Command."
  [["Remote Operations"
    ("a" "Add    Remote" magit-remote-add)
    ("r" "Rename Remote" magit-remote-rename)]]
  )

(transient-define-prefix mcge-magit-log-menu ()
  "Logs Command."
  [["Log"
    ("c" "Log Current" magit-log-current)
    ("l" "Log All"   magit-log-all)
    ("h" "Log Heads" magit-log-head)
    ("b" "Log Branch" magit-log-branches)
    ("f" "Log Current File" magit-log-buffer-file)]
   ]
  )

(transient-define-prefix mcge-magit-file-menu ()
  "Files Command."
  [["File"
    ("r" "File Rename" magit-file-rename)
    ("d" "File Delete" magit-file-delete)]
   ]
  )

(transient-define-prefix mcge-magit-diff-menu ()
  "Diff Command."
  [["Diff"
    ("d" "Diff file" magit-diff-buffer-file)]
   ]
  )

(provide 'init-magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-magit.el ends here
