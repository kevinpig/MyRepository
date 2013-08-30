(require 'util)
(require 'info)
(require 'unicad)

;;当光标在()中间时候，高亮匹配的()
(require 'highlight-parentheses)
(add-hook 'find-file-hooks 
		  '(lambda()
			 (highlight-parentheses-mode t)))

;;输入左边的括号后，自动补全右面的括号
(require 'autopair)
(autopair-global-mode)

;; cedet的设置
;;(load "cedet-settings")

;; 加载自动补全的配置
(require 'auto-complete-settings)
;; 使用company
(load "company-settings")

;; abbrev 简写模式
(setq-default abbrev-mode t)
(load "msf-abbrev-settings")
(load "yasnippet-settings")

(require 'shell-setting)

(load "rect-mark-settings")
(load "edit-settings")
(load "anything-settings")

;; 天气预报
(load "weather-settings")

;; 中文日历
(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-priority1-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-priority1-holidays)

(require 'hl-sexp)

;; Python IDE设置
(require 'python-setting)

;; 目录操作的设置
(load "dired-settings")

;; google protocol buffer mode
(require 'protobuf-mode)
(setq auto-mode-alist (cons '(".proto$" . protobuf-mode) auto-mode-alist))

;; sqlite 设置
;; (autoload 'sqlite-dump "sqlite-dump" nil t)
;; (modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
;; (add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))

;; (require 'mpc-mode)                     
;; (require 'ice-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;c-style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook
		  (lambda()
			(setq indent-tabs-mode nil)
			(c-set-style "stroustrup")
			;;开启自动新行和贪婪删除模式
			(c-toggle-hungry-state t)
			(font-lock-mode t)
			(setq semantic-show-unmatched-syntax-mode nil)
			))
(require 'doxygen_setting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dos Mode 用来编写bat文件
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'dos-mode "Dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.cmd$" . dos-mode))

;; 自动插入一些东西
(load "template-settings")
;; indent with spaces 

(dolist (hook (list 'sh-mode-hook 'mpc-mode-hook
                    'shell-mode-hook
                    'ice-mode-hook) )
  (add-hook hook
           (lambda()
            (setq indent-tabs-mode nil) )))
            
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)

(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode))
              auto-mode-alist))

(provide 'lrf-exbasic-config)
