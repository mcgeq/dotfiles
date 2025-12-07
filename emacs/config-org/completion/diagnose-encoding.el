;;; diagnose-encoding.el --- Diagnose Windows encoding issues -*- lexical-binding: t; -*-

;;; Commentary:
;; 诊断 Windows 平台编码问题的工具

;;; Code:

(defun mcg/diagnose-encoding ()
  "诊断 Windows 编码配置。"
  (interactive)
  (with-current-buffer (get-buffer-create "*Encoding Diagnosis*")
    (erase-buffer)
    (insert "=== Emacs 编码诊断报告 ===\n\n")
    
    ;; 系统信息
    (insert "## 系统信息\n")
    (insert (format "系统类型: %s\n" system-type))
    (insert (format "Emacs 版本: %s\n\n" emacs-version))
    
    ;; 编码设置
    (insert "## 编码设置\n")
    (insert (format "default-coding-system: %s\n" 
                    (or (default-value 'buffer-file-coding-system) "未设置")))
    (insert (format "locale-coding-system: %s\n" locale-coding-system))
    (insert (format "default-process-coding-system: %s\n" 
                    default-process-coding-system))
    (insert (format "file-name-coding-system: %s\n\n" 
                    file-name-coding-system))
    
    ;; find 程序
    (insert "## Find 程序\n")
    (insert (format "find-program: %s\n" (or find-program "未设置")))
    (if find-program
        (progn
          (insert (format "find 程序存在: %s\n" 
                          (if (file-exists-p find-program) "是" "否")))
          (when (file-exists-p find-program)
            (insert "find 版本:\n")
            (insert (shell-command-to-string 
                     (format "\"%s\" --version" find-program)))))
      (insert "使用系统默认 find\n"))
    (insert "\n")
    
    ;; process-coding-system-alist
    (insert "## 进程编码配置\n")
    (let ((find-coding (assoc "find" process-coding-system-alist)))
      (if find-coding
          (insert (format "find 编码: %s\n" find-coding))
        (insert "find 编码: 未配置\n")))
    (insert "\n")
    
    ;; PATH 中的 find
    (insert "## PATH 中的 find 程序\n")
    (let ((find-in-path (executable-find "find")))
      (if find-in-path
          (insert (format "PATH find: %s\n" find-in-path))
        (insert "PATH 中未找到 find\n")))
    
    ;; Git Bash find
    (insert "\n## Git Bash Find 检查\n")
    (let ((git-paths '("C:/Program Files/Git/usr/bin/find.exe"
                       "C:/Program Files (x86)/Git/usr/bin/find.exe"
                       "D:/Program Files/Git/usr/bin/find.exe"
                       "D:/Git/usr/bin/find.exe")))
      (dolist (path git-paths)
        (insert (format "%s: %s\n" 
                        path 
                        (if (file-exists-p path) "✓ 存在" "✗ 不存在")))))
    
    ;; 测试中文
    (insert "\n## 中文编码测试\n")
    (insert "测试字符串: 关于我们\n")
    (insert (format "UTF-8 编码: %s\n" 
                    (encode-coding-string "关于我们" 'utf-8)))
    (insert (format "GBK 编码: %s\n" 
                    (encode-coding-string "关于我们" 'gbk)))
    
    ;; 建议
    (insert "\n## 修复建议\n")
    (cond
     ((not (eq system-type 'windows-nt))
      (insert "✓ 非 Windows 系统，无需特殊配置\n"))
     
     ((not find-program)
      (insert "⚠ 未设置 find-program\n")
      (insert "建议: 在配置中设置 Git Bash find 路径\n"))
     
     ((not (file-exists-p find-program))
      (insert "✗ find-program 指向的文件不存在\n")
      (insert "建议: 检查 Git Bash 安装路径\n"))
     
     (t
      (insert "✓ find-program 配置正确\n")))
    
    (unless (assoc "find" process-coding-system-alist)
      (insert "⚠ find 进程编码未配置\n")
      (insert "建议: 添加 (cons '(\"find\" . (utf-8 . utf-8)) process-coding-system-alist)\n"))
    
    (insert "\n=== 诊断完成 ===\n")
    (insert "\n如需帮助，请将此报告发送给开发者。\n")
    
    (goto-char (point-min))
    (special-mode)
    (switch-to-buffer (current-buffer))))

(defun mcg/test-find-chinese ()
  "测试 find 命令对中文文件名的处理。"
  (interactive)
  (let* ((test-file (expand-file-name "测试文件.txt" temporary-file-directory))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8))
    
    ;; 创建测试文件
    (with-temp-file test-file
      (insert "这是一个测试文件\n"))
    
    ;; 测试 find
    (message "测试文件: %s" test-file)
    (message "find 输出:")
    (let ((output (shell-command-to-string
                   (format "\"%s\" \"%s\" -name \"*测试*\"" 
                           (or find-program "find")
                           temporary-file-directory))))
      (message "%s" output)
      (with-current-buffer (get-buffer-create "*Find Test*")
        (erase-buffer)
        (insert "=== Find 中文测试 ===\n\n")
        (insert (format "测试文件: %s\n" test-file))
        (insert (format "文件存在: %s\n" (if (file-exists-p test-file) "是" "否")))
        (insert (format "\nfind 命令: %s\n" (or find-program "系统 find")))
        (insert "\nfind 输出:\n")
        (insert output)
        (insert "\n如果输出为空或乱码，说明存在编码问题。\n")
        (goto-char (point-min))
        (switch-to-buffer (current-buffer))))
    
    ;; 清理
    (when (file-exists-p test-file)
      (delete-file test-file))))

(provide 'diagnose-encoding)
;;; diagnose-encoding.el ends here
