;;; lang-cpp -- Config for Modern CPP Font -*- lexical-binding: t; -*-

;; Filename: lang-cpp
;; Description: Config for Modern CPP Font
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: 2025-01-04 15:46:46
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Modern CPP Font
;;

;;; Installation:
;;
;; Put lang-cpp to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lang-cpp)
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

;;; Code:

(dolist (hook (list
               'c-mode-hook
               'c++-mode-hook
               'c-mode-common-hook
               ))
  (add-hook hook #'(lambda ()
                     (require 'cc-mode)
                     (require 'modern-cpp-font-lock)

                     (defun c-mode-style-setup ()
                       (interactive)
                       ;; cpp font lock.
                       (modern-c++-font-lock-global-mode t)

                       ;; base-style
                       (c-set-style "stroustrup")

                       ;; qt keywords and stuff ...
                       ;; set up indenting correctly for new qt kewords
                       (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                                      "\\|protected slot\\|private\\|private slot"
                                                      "\\)\\>")
                             c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                                      "\\|public slots\\|protected slots\\|private slots"
                                                      "\\)\\>[ \t]*:"))
                       (progn
                         ;; modify the colour of slots to match public, private, etc ...
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
                         ;; make new font for rest of qt keywords
                         (make-face 'qt-keywords-face)
                         (set-face-foreground 'qt-keywords-face "DeepSkyBlue1")
                         ;; qt keywords
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
                         (font-lock-add-keywords 'c++-mode
                                                 '(("\\<Q[A-Z][A-Za-z]\\>" . 'qt-keywords-face)))
                         ))
                     (c-mode-style-setup))))

(provide 'lang-cpp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-cpp.el ends here
