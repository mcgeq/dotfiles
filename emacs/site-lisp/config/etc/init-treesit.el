;;; init-treesit.el -- Config for Treesit -*- lexical-binding: t; -*-

;; Filename: init-treesit.el
;; Description: Config for Treesit
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By: <2025-01-09 Thu 18:08>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Treesit
;;

;;; Installation:
;;
;; Put init-treesit.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-treesit)
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

(require 'treesit)

;;; Code:
;; M-x `treesit-install-language-grammar` to install language grammar.
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (elixir "https://github.com/elixir-lang/tree-sitter-elixir" "main" "src" nil nil)
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (haskell "https://github.com/tree-sitter/tree-sitter-haskell" "master" "src" nil nil)
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (scala "https://github.com/tree-sitter/tree-sitter-scala" "master" "src" nil nil)
        (toml "https://github.com/tree-sitter/tree-sitter-toml" "master" "src" nil nil)
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))
        (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
        (mojo . ("https://github.com/HerringtonDarkholme/tree-sitter-mojo"))))

(setq major-mode-remap-alist
      '(
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (conf-toml-mode  . toml-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (rust-mode       . rust-ts-mode)
        (java-mode       . java-ts-mode)
        (clojure-mode    . clojure-ts-mode)
        (markdown-mode   . markdown-ts-mode)
        ))

(add-hook 'web-mode-hook #'(lambda ()
                               (let ((file-name (buffer-file-name)))
                                 (when file-name
                                   (treesit-parser-create
                                    (pcase (file-name-extension file-name)
                                      ("vue" 'vue)
                                      ("html" 'html)
                                      ("php" 'php))))
                                 )))

(defun add-treesit-parser-hooks (pairs)
"根据给定的 PAIRS 为多个模式添加 treesit-parse.
Pairs 是一个包含模式和语言对的列表"
(dolist (pair pairs)
  (add-hook (car pair) #'(lambda () (treesit-parser-create (cadr pair)))))
)

(setq treesit-parser-pairs
      '((markdown-ts-mode-hook markdown)
        (zig-mode-hook zig)
        (mojo-mode-hook mojo)
        (emacs-lisp-mode-hook elisp)
        (ielm-mode-hook elisp)
        (json-mode-hook json)
        (go-mode-hook go)
        (java-mode-hook java)
        (java-ts-mode-hook java)
        (clojure-mode-hook clojure)
        (clojure-ts-mode-hook clojure)
        (cider-repl-mode-hook clojure)
        (php-mode-hook php)
        (php-ts-mode-hook php)
        (haskell-mode-hook haskell)
        (kotlin-mode-hook kotlin)
        (lua-mode-hook lua)
        ))

;; 批量添加所有钩子
(add-treesit-parser-hooks treesit-parser-pairs)

  ;; (add-hook 'markdown-ts-mode-hook #'(lambda () (treesit-parser-create 'markdown)))
  ;; (add-hook 'zig-mode-hook #'(lambda () (treesit-parser-create 'zig)))
  ;; (add-hook 'mojo-mode-hook #'(lambda () (treesit-parser-create 'mojo)))
  ;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  ;; (add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
  ;; (add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
  ;; (add-hook 'go-mode-hook #'(lambda () (treesit-parser-create 'go)))
  ;; (add-hook 'java-mode-hook #'(lambda () (treesit-parser-create 'java)))
  ;; (add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))
  ;; (add-hook 'clojure-mode-hook #'(lambda () (treesit-parser-create 'clojure)))
  ;; (add-hook 'clojure-ts-mode-hook #'(lambda () (treesit-parser-create 'clojure)))
  ;; (add-hook 'cider-repl-mode-hook #'(lambda () (treesit-parser-create 'clojure)))
  ;; (add-hook 'php-mode-hook #'(lambda () (treesit-parser-create 'php)))
  ;; (add-hook 'php-ts-mode-hook #'(lambda () (treesit-parser-create 'php)))
  ;; (add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))
  ;; (add-hook 'haskell-mode-hook #'(lambda () (treesit-parser-create 'haskell)))
  ;; (add-hook 'kotlin-mode-hook #'(lambda () (treesit-parser-create 'kotlin)))
  ;; (add-hook 'lua-mode-hook #'(lambda () (treesit-parser-create 'lua)))
  ;; (add-hook 'C++-mode-hook #'(lambda () (treesit-parser-create 'cpp)))

;;; Code:

;;; ### auto-mode-alist ###
;;; --- 绑定扩展名到特定的模式
(defun add-to-alist (alist-var elt-cons &optional no-replace)
  "Add to the value of ALIST-VAR an element ELT-CONS if it isn't there yet.
If an element with the same car as the car of ELT-CONS is already present,
replace it with ELT-CONS unless NO-REPLACE is non-nil; if a matching
element is not already present, add ELT-CONS to the front of the alist.
The test for presence of the car of ELT-CONS is done with `equal'."
  (let ((existing-element (assoc (car elt-cons) (symbol-value alist-var))))
    (if existing-element
        (or no-replace
            (rplacd existing-element (cdr elt-cons)))
      (set alist-var (cons elt-cons (symbol-value alist-var)))))
  (symbol-value alist-var))

(dolist (elt-cons '(
                    ("\\.markdown" . markdown-mode)
                    ("\\.md" . markdown-mode)
                    ("\\.coffee$" . coffee-mode)
                    ("\\.iced$" . coffee-mode)
                    ("Cakefile" . coffee-mode)
                    ("\\.stumpwmrc\\'" . lisp-mode)
                    ("\\.[hg]s\\'" . haskell-mode)
                    ("\\.hi\\'" . haskell-mode)
                    ("\\.hs-boot\\'" . haskell-mode)
                    ("\\.chs\\'" . haskell-mode)
                    ("\\.l[hg]s\\'" . literate-haskell-mode)
                    ("\\.inc\\'" . asm-mode)
                    ("\\.max\\'" . maxima-mode)
                    ("\\.org\\'" . org-mode)
                    ("\\.cron\\(tab\\)?\\'" . crontab-mode)
                    ("cron\\(tab\\)?\\." . crontab-mode)
                    ("\\.a90\\'" . intel-hex-mode)
                    ("\\.hex\\'" . intel-hex-mode)
                    ("\\.py$" . python-mode)
                    ("/\\.php_cs\\(?:\\.dist\\)?\\'" . php-mode)
                    ("\\.\\(?:php\\.inc\\|stub\\)\\'" . php-mode)
                    ("\\.\\(?:php[s345]?\\|phtml\\)\\'" . php-mode-maybe)
                    ("SConstruct". python-mode)
                    ("\\.ml\\'" . tuareg-mode)
                    ("\\.mli\\'" . tuareg-mode)
                    ("\\.mly\\'" . tuareg-mode)
                    ("\\.mll\\'" . tuareg-mode)
                    ("\\.mlp\\'" . tuareg-mode)
                    ("\\.qml\\'" . qml-mode)
                    ("\\.jl\\'" . lisp-mode)
                    ("\\.asdf\\'" . lisp-mode)
                    ("CMakeLists\\.txt\\'" . cmake-mode)
                    ("\\.cmake\\'" . cmake-mode)
                    ("\\.vue" . web-mode)
                    ("\\.wxml" . web-mode)
                    ("\\.phtml\\'" . web-mode)
                    ("\\.jsp\\'" . web-mode)
                    ("\\.as[cp]x\\'" . web-mode)
                    ("\\.erb\\'" . web-mode)
                    ("\\.mustache\\'" . web-mode)
                    ("\\.djhtml\\'" . web-mode)
                    ("\\.html?\\'" . web-mode)
                    ("\\.coffee\\'" . coffee-mode)
                    ("\\.coffee.erb\\'" . coffee-mode)
                    ("\\.js.erb\\'" . js-mode)
                    ("\\.iced\\'" . coffee-mode)
                    ("\\.css\\'" . css-mode)
                    ("\\.wxss\\'" . css-mode)
                    ("Cakefile\\'" . coffee-mode)
                    ("\\.styl$" . sws-mode)
                    ("\\.jade" . jade-mode)
                    ("\\.go$" . go-mode)
                    ("\\.vala$" . vala-mode)
                    ("\\.vapi$" . vala-mode)
                    ("\\.rs$" . rust-mode)
                    ("\\.rs$" . rust-ts-mode)
                    ("\\.pro$" . qmake-mode)
                    ("\\.js$" . js-mode)
                    ("\\.wxs$" . js-mode)
                    ("\\.jsx$" . web-mode)
                    ("\\.lua$" . lua-mode)
                    ("\\.swift$" . swift-mode)
                    ("\\.l$" . flex-mode)
                    ("\\.y$" . bison-mode)
                    ("\\.pdf$" . pdf-view-mode)
                    ("\\.ts$" . typescript-mode)
                    ("\\.tsx$" . typescript-mode)
                    ("\\.cpp$" . c++-mode)
                    ("\\.h$" . c++-mode)
                    ("\\.ll$" . llvm-mode)
                    ("\\.bc$" . hexl-mode)
                    ("\\.nim$" . nim-mode)
                    ("\\.nims$" . nim-mode)
                    ("\\.nimble$" . nim-mode)
                    ("\\.nim.cfg$" . nim-mode)
                    ("\\.exs$" . elixir-mode)
                    ("\\.json$" . json-mode)
                    ("\\.clj$" . clojure-ts-mode)
                    ("\\.dart$" . dart-mode)
                    ("\\.zig$" . zig-mode)
                    ("\\.kt$" . kotlin-mode)
                    ("\\.mojo$" . mojo-mode)
                    ("\\.fs$" . fsharp-mode)
                    ))
  (add-to-alist 'auto-mode-alist elt-cons))

(add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))

;;; ### Auto-fill ###
;;; --- 自动换行
(setq default-fill-column 100)          ;默认显示 100列就换行
(dolist (hook (list
               'after-text-mode-hook
               'message-mode-hook
               ))
  (add-hook hook #'(lambda () (auto-fill-mode 1))))

;;; Mode load.
(autoload 'cmake-mode "cmake-mode")
(autoload 'qml-mode "qml-mode")
(autoload 'php-mode "php-mode")
(autoload 'php-mode-maybe "php-mode")
(autoload 'coffee-mode "coffee-mode")
(autoload 'sws-mode "sws-mode")
(autoload 'jade-mode "jade-mode")
(autoload 'vala-mode "vala-mode")
(autoload 'rust-mode "rust-mode")
(autoload 'qmake-mode "qmake-mode")
(autoload 'swift-mode "swift-mode")
(autoload 'rjsx-mode "rjsx-mode")
(autoload 'flex-mode "flex")
(autoload 'bison-mode "bison")
(autoload 'llvm-mode "llvm-mode")
(autoload 'typescript-mode "typescript-mode")
(autoload 'elixir-mode "elixir-mode")
(autoload 'json-mode "json-mode")
(autoload 'clojure-ts-mode "clojure-ts-mode")
(autoload 'dart-mode "dart-mode")
(autoload 'zig-mode "zig-mode")
(autoload 'kotlin-mode "kotlin-mode")
(autoload 'mojo-mode "mojo")
(autoload 'fsharp-mode "fsharp-mode")
(autoload 'lua-mode "lang-lua")

(provide 'init-treesit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treesit.el ends here
