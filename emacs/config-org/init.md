- [init.el](#org743b2a3)
  - [Headers](#org0e0a422)
  - [Require](#orgd66dd28)
    - [Custom const variable and function](#orgef01a62)
    - [Emacs base config](#orgac61f5c)
  - [Code](#org3d2ba88)
  - [Ends](#org21b20a5)


<a id="org743b2a3"></a>

# init.el


<a id="org0e0a422"></a>

## Headers

```emacs-lisp
;;; init.el -- Config for Emacs Start -*- lexical-binding: t; -*-

;; Filename: init.el
;; Description: Config for Emacs Start
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcgeq <mcgeq@outlook.com>
;; Last Modified:  <2025-02-16 Sun 11:34>
;; Keywords:
;; Compatibility: 31.0.50

;;; Commentary:
;;
;; Config for Emacs Start
;;

;;; Installation:
;;
;; Put init.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init)
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

```


<a id="orgd66dd28"></a>

## Require


<a id="orgef01a62"></a>

### Custom const variable and function

```emacs-lisp
;;; Require:
;; Hide tools menu vertical-scroll
;; (push '(menu-bar-lines   . 0) default-frame-alist)
;; (push '(tool-bar-lines    . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(require 'hydra)
(require 'init-const)
(require 'init-font)
(require 'init-function)
(require 'lazy-load)
(require 'init-ui)

```


<a id="orgac61f5c"></a>

### Emacs base config

```emacs-lisp
(let (;; 加速启动
        (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件
          (file-name-handler-alist nil))
  (require 'init-builtin)
  (require 'init-sort-tab)
  (require 'init-doom-modeline)
  (require 'init-auto-save)
  (require 'init-line-number)
  (require 'init-generic)
  (require 'init-editor)
  (require 'init-marginalia)
  (require 'init-vertico)
  (require 'init-keymaps)
  (require 'init-lsp-bridge)
  (require 'init-recentf)
  (require 'init-wraplish)
  (require 'init-fingertip)
  (require 'init-indent)
  (require 'init-treesit)
  (require 'init-symbol-overlay)
  (require 'init-rime)

  ;; Delay loading
  (run-with-timer
   1 nil
   #'(lambda ()
       (require 'init-yasnippet)
       (require 'init-helpful)
       (require 'init-org)
       (require 'init-org-agenda)
       (require 'init-consult-todo)
       (require 'init-org-numbering)
       (require 'init-markdown)
       (require 'init-grip-mode)
       ))
  )
```


<a id="org3d2ba88"></a>

## Code

```emacs-lisp

;;; Code:
;; 绑定到保存前钩子
(add-hook 'before-save-hook 'mcg/org-update-file-headers)
```


<a id="org21b20a5"></a>

## Ends

```emacs-lisp
(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
```
