(server-start);;
(add-hook 'kill-emacs-hook	 ;;
	(lambda()	 ;;
	(if (file-exists-p "~/.emacs.d/server/server") ;;
	(delete-file "~/.emacs.d/server/server"))))	 ;;

;; ����Ŀ¼����Ŀ¼
(defun my-add-subdirs-to-load-path(dir)
  "Rescusive add directories to load-path"
  (let ((default-directory (file-name-as-directory dir)))
	(add-to-list 'load-path dir)
	(normal-top-level-add-subdirs-to-load-path)))

(defconst my-emacs-path "~/.emacs.d/" "�ҵ�emacs��������ļ���·��")
(defconst my-emacs-my-lisps-path  (concat my-emacs-path "my-lisps/") "���Լ�д��emacs lisp����·��")
(defconst my-emacs-lisps-path     (concat my-emacs-path "lisps/") "�����ص�emacs lisp����·��")
(defconst my-emacs-templates-path (concat my-emacs-path "templates/") "Path for templates")

;; ������������Ŀ¼��������Ŀ¼
(my-add-subdirs-to-load-path my-emacs-lisps-path)
(my-add-subdirs-to-load-path my-emacs-my-lisps-path)

;;�������ȫ�����ʼ����ڷ��ʼ�ʱ�����õ�
(setq user-full-name "kevin_lrf")
(setq user-mail-address "ruifei.liu@united-imaging.com")

(defconst is-before-emacs-21 (>= 21 emacs-major-version) "�Ƿ���emacs 21����ǰ�İ汾")
(defconst is-after-emacs-23  (<= 23 emacs-major-version) "�Ƿ���emacs 23���Ժ�İ汾")

;; ����Htpp���ʵĴ���
(setq url-proxy-services '(("http" . "proxysh.zte.com.cn:80")))

(defvar last-region-beg     nil "Beginning of last region.")
(defvar last-region-end     nil "End of last region.")
(defvar last-region-is-rect nil "Last region is rectangle or not.")
(defvar last-region-use-cua nil "Last region use CUA mode or not.")

(require 'lrf-basic-config)		;�������������ã��������κ���չ��lisp��
(require 'lrf-exbasic-config)	;�������ã���Ҫ��������չ��lisp��
(require 'lrf-theme-config)		;����

;; �ǳ�����鿴emacs�����Ĳ��
(require 'describe-symbol)
(require 'find-symbol)
(load "describe-find-symbol-settings")

;; �����Զ���Ŀ�ݼ�����
(load "lrf-key-map")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diredp-date-time ((((type tty)) :foreground "yellow") (t :foreground "goldenrod1")))
 '(diredp-dir-heading ((((type tty)) :background "yellow" :foreground "blue") (t :background "Pink" :foreground "DarkOrchid1")))
 '(diredp-display-msg ((((type tty)) :foreground "blue") (t :foreground "cornflower blue"))))
