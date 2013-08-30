(require 'util)
(require 'info)
(require 'unicad)

;;�������()�м�ʱ�򣬸���ƥ���()
(require 'highlight-parentheses)
(add-hook 'find-file-hooks 
		  '(lambda()
			 (highlight-parentheses-mode t)))

;;������ߵ����ź��Զ���ȫ���������
(require 'autopair)
(autopair-global-mode)

;; cedet������
;;(load "cedet-settings")

;; �����Զ���ȫ������
(require 'auto-complete-settings)
;; ʹ��company
(load "company-settings")

;; abbrev ��дģʽ
(setq-default abbrev-mode t)
(load "msf-abbrev-settings")
(load "yasnippet-settings")

(require 'shell-setting)

(load "rect-mark-settings")
(load "edit-settings")
(load "anything-settings")

;; ����Ԥ��
(load "weather-settings")

;; ��������
(require 'cal-china-x)
(setq mark-holidays-in-calendar t)
(setq cal-china-x-priority1-holidays cal-china-x-chinese-holidays)
(setq calendar-holidays cal-china-x-priority1-holidays)

(require 'hl-sexp)

;; Python IDE����
(require 'python-setting)

;; Ŀ¼����������
(load "dired-settings")

;; google protocol buffer mode
(require 'protobuf-mode)
(setq auto-mode-alist (cons '(".proto$" . protobuf-mode) auto-mode-alist))

;; sqlite ����
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
			;;�����Զ����к�̰��ɾ��ģʽ
			(c-toggle-hungry-state t)
			(font-lock-mode t)
			(setq semantic-show-unmatched-syntax-mode nil)
			))
(require 'doxygen_setting)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dos Mode ������дbat�ļ�
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'dos-mode "Dos" "Edit Dos scripts." t)
(add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode))
(add-to-list 'auto-mode-alist '("\\.cmd$" . dos-mode))

;; �Զ�����һЩ����
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
