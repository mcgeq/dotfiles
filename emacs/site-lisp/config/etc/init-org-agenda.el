;;; init-org-agenda.el -- Config for Org Agenda -*- lexical-binding: t; -*-

;; Filename: init-org-agenda.el
;; Description: Config for Org Agenda
;; Author: mcge <mcgeq@outlook.com>
;; Copyright (C) 2024, mcge, all rights reserved.
;; Create   Date: 2025-01-04 15:00:00
;; Version: 0.1
;; Modified   By:  mcge <mcgeq@outlook.com>
;; Last Modified:  <2025-01-10 Fri 10:14>
;; Keywords:
;; Compatibility: GNU Emacs 31.0.50

;;; Commentary:
;;
;; Config for Org Agenda
;;

;;; Installation:
;;
;; Put init-org-agenda.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-org-agenda)
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
(require 'org-agenda)

;;; Code:
;; 设置 org agenda 展示的文件
(setq org-agenda-files (list mcgemacs-org-agenda-files))

(setq agenda-used-time-grid t)
(setq org-agenda-use-time-grid t)

;; 设置时间线的当前时间指示串
(setq org-agenda-current-time-string "⏰------------now")

;; 设置 org-agenda 格式
(setq org-agenda-format-date 'org-agenda-format-date-aligned)
(defun org-agenda-format-date-aligned (date)
  "Format a DATE string for display in the daily/weekly agenda, or timeline.

This function makes sure that dates are aligned for easy reading."
  (require 'cal-iso)
  (let* ((dayname (aref cal-china-x-days
                        (calendar-day-of-week date)))   ;; 周几的名字
         (day (cadr date))                          ;; 日期
         (month (car date))                        ;; 月份
         (year (nth 2 date))                       ;; 年份
         (day-of-week (calendar-day-of-week date)) ;; 星期几
         (iso-week (org-days-to-iso-week
                    (calendar-absolute-from-gregorian date)))  ;; ISO 周号
         (cn-date (calendar-chinese-from-absolute (calendar-absolute-from-gregorian date))) ;; 农历
         (cn-month (cl-caddr cn-date))
         (cn-day (cl-cadddr cn-date))
         (cn-month-string (concat (aref cal-china-x-month-name
                                        (1- (floor cn-month)))
                                  (if (integerp cn-month)
                                      ""
                                    "（闰月）")))
         (cn-day-string (aref cal-china-x-day-name
                              (1- cn-day)))
         ;; 计算该日期是今年的第几天
         (absolute-date (calendar-absolute-from-gregorian date))
         (start-of-year (calendar-absolute-from-gregorian (list 1 1 year)))
         ;; 计算“今年的第几天”，从 1 月 1 日算起
         (day-of-year (+ 1 (- absolute-date start-of-year)))  ;; 修改：+1 保证第一天是第 1 天
         (extra (format " 农历%s%s%s 今年第 %02d 周 第%03d 天"
                        cn-month-string  ;; 始终显示农历月份
                        cn-day-string    ;; 始终显示农历日
                        (if (integerp cn-month) "" "[闰]") ;; 只在有闰月时加上[闰]
                        iso-week
                        day-of-year)))  ;; 显示天数
    (format "%04d-%02d-%02d 星期%s%s\n"
            year month day dayname extra)))

;; or-agenda-time-grid 八分图
;; #+tblname: 八分图时间段的概念命名
;; |----+-----------+----------+------------------|
;; | No | 时间段    | 维基命名 | 自定义命名       |
;; |----+-----------+----------+------------------|
;; |  1 | 0 时~3 时   | 凌晨     | 午夜(夜)         |
;; |  2 | 3 时~6 时   | 拂晓     | 破晓(破,dawn)    |
;; |  3 | 6 时~9 时   | 早晨     | 早晨(早,morning) |
;; |  4 | 9 时~12 时  | 午前     | 明昼(明)         |
;; |  5 | 12 时~15 时 | 午后     | 午后(午)         |
;; |  6 | 15 时~18 时 | 傍晚     | 下午(下)         |
;; |  7 | 18 时~21 时 | 薄暮     | 向晚(向,Dusk)    |
;; |  8 | 21 时~24 时 | 深夜     | 深夜(深,Night)   |
;; |----+-----------+----------+------------------|
(setq org-agenda-time-grid (quote ((daily today require-timed)
                                     (300
                                      600
                                      900
                                      1200
                                      1500
                                      1800
                                      2100
                                      2400)
                                     "......"
                                     "-----------------------------------------------------"
                                     )))

  (setq org-agenda-include-diary t)
  (setq org-agenda-diary-file mcgeamcs-org-agenda-diary-file)
  (setq diary-file mcgeamcs-org-agenda-diary-file)

    ;; 日程视图的前缀设置
    (setq org-agenda-prefix-format '((agenda . " %i %-25:c %5t %s")
                                     (todo   . " %i %-25:c ")
                                     (tags   . " %i %-25:c ")
                                     (search . " %i %-25:c ")))
  ;; 对于计划中的任务在视图里的显示
  (setq org-agenda-scheduled-leaders
        '("计划 " "应在%02d 天前开始 "))
  ;; 对于截止日期的任务在视图里的显示
  (setq org-agenda-deadline-leaders
        '("截止 " "还有%02d 天到期 " "已经过期%02d 天 "))

  (setq org-time-stamp-formats '("<%Y-%m-%d %A>" . "<%Y-%m-%d %A %H:%M>"))
  ;; 不同日程类别间的间隔
  (setq org-cycle-separator-lines 2)
  ;; 日记插入精确时间戳
  (setq org-agenda-insert-diary-extract-time t)
  ;; q 退出时删除 agenda 缓冲区
  (setq org-agenda-sticky t)
  ;; 时间不足位时前面加 0
  (setq org-agenda-time-leading-zero t)
  ;; 标签显示的位置，第 80 列往前右对齐
  (setq org-agenda-tags-column -80)
  ;; 提前 3 天截止日期到期告警
  (setq org-deadline-warning-days 3)

  ;; 设置本地坐标
  (setq calendar-longitude 116.9962)
  (setq calendar-latitude 39.91)

  ;;Sunrise and Sunset
  ;;日出而作, 日落而息
  (defun diary-sunrise ()
    (let ((dss (diary-sunrise-sunset)))
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ",")
        (buffer-substring (point-min) (match-beginning 0)))))

  (defun diary-sunset ()
    (let ((dss (diary-sunrise-sunset))
          start end)
      (with-temp-buffer
        (insert dss)
        (goto-char (point-min))
        (while (re-search-forward " ([^)]*)" nil t)
          (replace-match "" nil nil))
        (goto-char (point-min))
        (search-forward ", ")
        (setq start (match-end 0))
        (search-forward " at")
        (setq end (match-beginning 0))
        (goto-char start)
        (capitalize-word 1)
        (buffer-substring start end))))

  (setq calendar-chinese-celestial-stem
        ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
  (setq calendar-chinese-terrestrial-branch
        ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

;;Day info
;; 日出而作, 日落而息
;; %%(diary-sunrise)
;; %%(diary-sunset)
;; %%(diary-lunar-phases)
;;
;; %%(diary-iso-date)
;;中国农历
;; %%(diary-chinese-date)

(provide 'init-org-agenda)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org-agenda.el ends here
