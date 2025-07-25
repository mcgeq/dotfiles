;;; init-recentf.el -- Config for Recentf -*- lexical-binding: t; -*-

;; Filename: init-recentf.el
;; Description: Config for Recentf
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:22>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Recentf
;;

;;; Installation:
;;
;; Put init-recentf.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-recentf)
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
(require 'recentf)

;;; Code:

(add-hook 'after-init-hook 'recentf-mode)

(setq recentf-max-saved-items 300)
(setq recentf-auto-cleanup    'never)
(setq recentf-filename-handlers '(abbreviate-file-name))

(setq recentf-exclude `(,@(cl-loop for f in `(,package-user-dir
                                        ;,no-littering-var-directory
                                        ;,no-littering-etc-directory)
                                              )
                                   collect (abbreviate-file-name f))
                        ;; Folders on MacOS start
                        "^/private/recentfp/"
                        "^/var/folders/"
                        ;; Folders on MacOS end
                        ".cache"
                        ".cask"
                        ".elfeed"
                        "elfeed"
                        "bookmarks"
                        "cache"
                        "ido.*"
                        "persp-confs"
                        "recentf"
                        "undo-tree-hist"
                        "url"
                        "^/recentfp/"
                        "/ssh\\(x\\)?:"
                        "/su\\(do\\)?:"
                        "^/usr/include/"
                        "/TAGS\\'"
                        "COMMIT_EDIRECENTFSG\\'"))

(provide 'init-recentf)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-recentf.el ends here
