;;; init-org.el -- Config for Emacs Font -*- lexical-binding: t; -*-

;; Filename: init-org.el
;; Description: Config for Emacs Font
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcgeq <mcgeq@outlook.com>
;; Last Modified:  2025-03-21 21:03:22
;; Keywords:
;; Compatibility: 31.0.50

;;; Commentary:
;;
;; Config for Font
;;

;;; Installation:
;;
;; Put init-org.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-org)
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
(require 'visual-fill-column)
(require 'mixed-pitch)
(require 'org-superstar)
(require 'org-appear)
(require 'cal-china-x)
(require 'ox)

;;; Code:

(setq org-ellipsis " ⭍"
      org-tags-column 0
      org-log-into-drawer t
      org-pretty-entities t
      org-startup-indented t
      org-hide-leading-stars nil
      org-hide-emphasis-markers t
      org-image-actual-width '(800)
      org-startup-with-inline-images t
      org-indent-mode-turns-on-hiding-stars nil)

;; done close time
(setq org-log-done 'time)

;; TOOD 的关键词设置，可以设置不同的组
(setq org-todo-keywords '((sequence
                           "TODO(t)"
                           "HOLD(h!)"
                           "WIP(i!)"
                           "WAIT(w!)"
                           "|"
                           "DONE(d!)"
                           "CANCELLED(c@/!)")
                          (sequence
                           "REPORT(r)"
                           "BUG(b)"
                           "KNOWNCAUSE(k)"
                           "|"
                           "FIXED(f!)")))
;; TODO 关键词的样式设置
(setq org-todo-keyword-faces '(("TODO"       :foreground "#7c7c75" :weight bold)
                               ("HOLD"       :foreground "#feb24c" :weight bold)
                               ("WIP"        :foreground "#0098dd" :weight bold)
                               ("WAIT"       :foreground "#9f7efe" :weight bold)
                               ("DONE"       :foreground "#50a14f" :weight bold)
                               ("CANCELLED"  :foreground "#ff6480" :weight bold)
                               ("REPORT"     :foreground "magenta" :weight bold)
                               ("BUG"        :foreground "red"     :weight bold)
                               ("KNOWNCAUSE" :foreground "yellow"  :weight bold)
                               ("FIXED"      :foreground "green"   :weight bold)))

;; 当标题行状态变化时标签同步发生的变化
;; Moving a task to CANCELLED adds a CANCELLED tag
;; Moving a task to WAIT adds a WAIT tag
;; Moving a task to HOLD adds WAIT and HOLD tags
;; Moving a task to a done state removes WAIT and HOLD tags
;; Moving a task to TODO removes WAIT, CANCELLED, and HOLD tags
;; Moving a task to DONE removes WAIT, CANCELLED, and HOLD tags
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAIT" ("WAIT" . t))
              ("HOLD" ("WAIT") ("HOLD" . t))
              (done ("WAIT") ("HOLD"))
              ("TODO" ("WAIT") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAIT") ("CANCELLED") ("HOLD")))))

;; 重复执行时加上时间戳
(setq org-log-repeat 'time)
;; Deadline 修改时加上一条记录
(setq org-log-redeadline 'note)
;; Schedule 修改时加上一条记录
(setq org-log-reschedule 'note)
;; 以抽屉的方式记录
(setq org-log-into-drawer t)
;; 紧接着标题行或者计划/截止时间戳后加上记录抽屉
(setq org-log-state-notes-insert-after-drawers nil)

(custom-set-faces
 '(org-level-1 ((t (:height 1.15))))
 '(org-level-2 ((t (:height 1.13))))
 '(org-level-3 ((t (:height 1.11))))
 '(org-level-4 ((t (:height 1.09))))
 '(org-level-5 ((t (:height 1.07))))
 '(org-level-6 ((t (:height 1.05))))
 '(org-level-7 ((t (:height 1.03))))
 '(org-level-8 ((t (:height 1.01))))
 '(org-tag ((t (:inherit 'fixed-pitch))))
 '(org-date ((t (:inherit 'fixed-pitch))))
 '(org-todo ((t (:inherit 'fixed-pitch))))
 '(org-done ((t (:inherit 'fixed-pitch))))
 '(org-drawer ((t (:inherit 'fixed-pitch))))
 '(org-ellipsis ((t (:inherit 'fixed-pitch))))
 '(org-property-value ((t (:inherit 'fixed-pitch))))
 '(org-special-keyword ((t (:inherit 'fixed-pitch))))
 '(org-headline-done ((t (:inherit 'variable-pitch)))))

;; visual-fill-column
(setq visual-fill-column-width 88)

;; mixed-pitch
(custom-set-faces
 '(default ((t (:font "FiraCode Nerd Font Mono"))))
 '(fixed-pitch ((t (:font "FiraCode Nerd Font Mono" :height 1.0))))
 '(variable-pitch ((t (:font "Noto Serif" :height 1.0)))))

;; org-superstar
(setq org-superstar-leading-bullet ?\s
      org-superstar-special-todo-items t
      org-superstar-item-bullet-alist '((43 . "⬧") (45 . "⬨"))
      org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷"))

(custom-set-faces
 '(org-superstar-item ((t (:inherit 'fixed-pitch))))
 '(org-superstar-header-bullet ((t (:height 232 :inherit 'fixed-pitch)))))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local line-spacing 2)
            (visual-line-mode)
            (visual-fill-column-mode)
            (mixed-pitch-mode)
            (org-superstar-mode)
            (org-appear-mode)))

(defun mg/org-font-lock-drawer (limit)
  (when (or (re-search-forward "SCHEDULED:\\(\\(.\\|\n\\)+:\s*\n\\)" limit t)
            (re-search-forward ".+?:\s+\\(.+\\)$" limit t))
    (let ((beg (match-beginning 1))
          (end (match-end 1)))
      (put-text-property beg end 'face 'fixed-pitch)
      (put-text-property (match-beginning 0) (match-end 0) 'font-lock-multiline 't)
      (goto-char end))))

(font-lock-add-keywords 'org-mode
                        '(mg/org-font-lock-drawer))

(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                           (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))))

;; calendar
;; 配置 calendar
(with-eval-after-load 'calendar
  ;; 设置标记今天
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today)

  ;; 设置是否显示中国节日
  (setq calendar-chinese-all-holidays-flag nil)
  ;; 设置是否显示节日
  (setq calendar-mark-holidays-flag t)
  ;; 设置是否显示日记条目（使用 Org 日记）
  (setq calendar-mark-diary-entries-flag nil)
  ;; 设置时区的显示方式，数字方式（如 +0800）
  (setq calendar-time-zone-style 'numeric)
  ;; 设置日期显示方式：年/月/日
  (setq calendar-date-style 'iso)
  ;; 设置中文天干地支
  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (setq calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
  ;; 设置中文月份名称
  (setq calendar-month-name-array
        ["一月" "二月" "三月" "四月" "五月" "六月" "七月" "八月" "九月" "十月" "十一月" "十二月"])
  ;; 设置星期标题显示
  (setq calendar-day-name-array
        ["日" "一" "二" "三" "四" "五" "六"])
  ;; 设置周一为一周的第一天
  (setq calendar-week-start-day 1))

;; parse time
;; 时间解析增加中文拼音
(with-eval-after-load 'parse-time
  (setq parse-time-months
        (append '(("yiy" . 1) ("ery" . 2) ("sany" . 3)
                  ("siy" . 4) ("wuy" . 5) ("liuy" . 6)
                  ("qiy" . 7) ("bay" . 8) ("jiuy" . 9)
                  ("shiy" . 10) ("shiyiy" . 11) ("shiery" . 12)
                  ("yiyue" . 1) ("eryue" . 2) ("sanyue" . 3)
                  ("siyue" . 4) ("wuyue" . 5) ("liuyue" . 6)
                  ("qiyue" . 7) ("bayue" . 8) ("jiuyue" . 9)
                  ("shiyue" . 10) ("shiyiyue" . 11) ("shieryue" . 12))
                parse-time-months))

  (setq parse-time-weekdays
        (append '(("zri" . 0) ("zqi" . 0)
                  ("zyi" . 1) ("zer" . 2) ("zsan" . 3)
                  ("zsi" . 4) ("zwu" . 5) ("zliu" . 6)
                  ("zr" . 0) ("zq" . 0)
                  ("zy" . 1) ("ze" . 2) ("zs" . 3)
                  ("zsi" . 4) ("zw" . 5) ("zl" . 6))
                parse-time-weekdays)))

;; cal-china-x
;; 中国节日设置
(with-eval-after-load 'cal-china-x
  ;; 设置重要节日
  (setq calendar-mark-holidays-flag t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)

  ;; 设置所有节日
  (setq cal-china-x-general-holidays
        '(;; 公历节日
          (holiday-fixed 1 1 "元旦")
          (holiday-fixed 2 14 "情人节")
          (holiday-fixed 3 8 "妇女节")
          (holiday-fixed 3 14 "白色情人节")
          (holiday-fixed 4 1 "愚人节")
          (holiday-fixed 5 1 "劳动节")
          (holiday-fixed 5 4 "青年节")
          (holiday-float 5 0 2 "母亲节")
          (holiday-fixed 6 1 "儿童节")
          (holiday-float 6 0 3 "父亲节")
          (holiday-fixed 9 10 "教师节")
          (holiday-fixed 10 1 "国庆节")
          (holiday-fixed 10 2 "国庆节")
          (holiday-fixed 10 3 "国庆节")
          (holiday-fixed 10 24 "程序员节")
          (holiday-fixed 11 11 "双 11 购物节")
          (holiday-fixed 12 25 "圣诞节")
          ;; 农历节日
          (holiday-lunar 12 30 "春节" 0)
          (holiday-lunar 1 1 "春节" 0)
          (holiday-lunar 1 2 "春节" 0)
          (holiday-lunar 1 15 "元宵节" 0)
          (holiday-solar-term "清明" "清明节")
          (holiday-solar-term "小寒" "小寒")
          (holiday-solar-term "大寒" "大寒")
          (holiday-solar-term "立春" "立春")
          (holiday-solar-term "雨水" "雨水")
          (holiday-solar-term "惊蛰" "惊蛰")
          (holiday-solar-term "春分" "春分")
          (holiday-solar-term "谷雨" "谷雨")
          (holiday-solar-term "立夏" "立夏")
          (holiday-solar-term "小满" "小满")
          (holiday-solar-term "芒种" "芒种")
          (holiday-solar-term "夏至" "夏至")
          (holiday-solar-term "小暑" "小暑")
          (holiday-solar-term "大暑" "大暑")
          (holiday-solar-term "立秋" "立秋")
          (holiday-solar-term "处暑" "处暑")
          (holiday-solar-term "白露" "白露")
          (holiday-solar-term "秋分" "秋分")
          (holiday-solar-term "寒露" "寒露")
          (holiday-solar-term "霜降" "霜降")
          (holiday-solar-term "立冬" "立冬")
          (holiday-solar-term "小雪" "小雪")
          (holiday-solar-term "大雪" "大雪")
          (holiday-solar-term "冬至" "冬至")
          (holiday-lunar 5 5 "端午节" 0)
          (holiday-lunar 8 15 "中秋节" 0)
          (holiday-lunar 7 7 "七夕情人节" 0)
          (holiday-lunar 12 8 "腊八节" 0)
          (holiday-lunar 9 9 "重阳节" 0)))

  ;; 设置日历的节日，通用节日已经包含了所有节日
  (setq calendar-holidays (append cal-china-x-general-holidays
                                  cal-china-x-important-holidays
                                  )))

;; 自动初始化 cal-china-x
(add-hook 'after-init-hook 'cal-china-x-setup)

;; viewer
(eval-after-load "org"
  '(require 'ox-gfm nil t))
(add-to-list 'org-export-backends 'md)
(add-to-list 'org-export-backends 'gfm)

;; 当 org 模块加载完毕后设置行间距
(with-eval-after-load 'org
  (setq line-spacing 0.25)
  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))))
(font-lock-add-keywords
 'org-mode
 '(("^ +\\([-*]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▻"))))))

;; ================================
;; 在 org mode 里美化字符串
;; ================================
(defun mcg/org-prettify-symbols ()
  (setq prettify-symbols-alist
        (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                '(
                  ("#+begin_src"      . "↪")         ; 21AA 9998
                  ("#+end_src"        . "↩")         ; 21A9 □ 9633
                  ("#+begin_example"  . 129083)       ; 🠻
                  ("#+end_example"    . 129081)       ; 🠹
                  ("#+results:"       . 9776)         ; ☰
                  ("#+attr_latex:"    . "🄛")
                  ("#+attr_html:"     . "🄗")
                  ("#+attr_org:"      . "🄞")
                  ("#+name:"          . "🄝")         ; 127261
                  ("#+caption:"       . "🄒")         ; 127250
                  ("#+date:"          . "📅")         ; 128197
                  ("#+author:"        . "💁")         ; 128100
                  ("#+setupfile:"     . 128221)       ; 📝
                  ("#+email:"         . 128231)       ; 📧
                  ("#+startup:"       . 10034)        ; ✲
                  ("#+options:"       . 9965)         ; ⛭
                  ("#+title:"         . 10162)        ; ➲
                  ("#+subtitle:"      . 11146)        ; ⮊
                  ("#+downloaded:"    . 8650)         ; ⇊
                  ("#+language:"      . 128441)       ; 🖹
                  ("#+begin_quote"    . 187)          ; »
                  ("#+end_quote"      . 171)          ; «
                  ("#+begin_results"  . 8943)         ; ⋯
                  ("#+end_results"    . 8943)         ; ⋯
                  (":PROPERTIES:"     . "⥼")
                  (":HEADER-ARGS:"    . "Э")
                  (":END:"            . "⥽")
                  )))
  (setq prettify-symbols-unprettify-at-point t)
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook 'mcg/org-prettify-symbols)

;; https://emacs-china.org/t/org-mode/22313?u=vagrantjoker
;; 解决中文标记前后空格的问题
(font-lock-add-keywords 'org-mode
                        '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9]*?[/+*_=~]\\( \\)?\\cc?"
                          (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                          ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9]*?[/+*_=~]\\( \\)\\cc"
                           (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                        'append)

(with-eval-after-load 'org
  (defun eli-strip-ws-maybe (text _backend _info)
    (let* ((text (replace-regexp-in-string
                  "\\(\\cc\\) *\n *\\(\\cc\\)"
                  "\\1\\2" text));; remove whitespace from line break
           ;; remove whitespace from `org-emphasis-alist'
           (text (replace-regexp-in-string "\\(\\cc\\) \\(.*?\\) \\(\\cc\\)"
                                           "\\1\\2\\3" text))
           ;; restore whitespace between English words and Chinese words
           (text (replace-regexp-in-string "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)"
                                           "\\1 \\2 \\3" text)))
      text))
  (add-to-list 'org-export-filter-paragraph-functions #'eli-strip-ws-maybe))

(add-hook 'org-mode-hook (lambda () (setq line-spacing 0.4)))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
