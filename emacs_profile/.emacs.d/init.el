(server-start);;
(add-hook 'kill-emacs-hook	 ;;
	(lambda()	 ;;
	(if (file-exists-p "~/.emacs.d/server/server") ;;
	(delete-file "~/.emacs.d/server/server"))))	 ;;

;; 加载目录和子目录
(defun my-add-subdirs-to-load-path(dir)
  "Rescusive add directories to load-path"
  (let ((default-directory (file-name-as-directory dir)))
	(add-to-list 'load-path dir)
	(normal-top-level-add-subdirs-to-load-path)))

(defconst my-emacs-path "~/.emacs.d/" "我的emacs相关配置文件的路径")
(defconst my-emacs-my-lisps-path  (concat my-emacs-path "my-lisps/") "我自己写的emacs lisp包的路径")
(defconst my-emacs-lisps-path     (concat my-emacs-path "lisps/") "我下载的emacs lisp包的路径")
(defconst my-emacs-templates-path (concat my-emacs-path "templates/") "Path for templates")

;; 加载下面两个目录的所有子目录
(my-add-subdirs-to-load-path my-emacs-lisps-path)
(my-add-subdirs-to-load-path my-emacs-my-lisps-path)

;;设置你的全名和邮件，在发邮件时可以用到
(setq user-full-name "kevin_lrf")
(setq user-mail-address "ruifei.liu@united-imaging.com")

(defconst is-before-emacs-21 (>= 21 emacs-major-version) "是否是emacs 21或以前的版本")
(defconst is-after-emacs-23  (<= 23 emacs-major-version) "是否是emacs 23或以后的版本")

;; 设置Htpp访问的代理
(setq url-proxy-services '(("http" . "proxysh.zte.com.cn:80")))

(defvar last-region-beg     nil "Beginning of last region.")
(defvar last-region-end     nil "End of last region.")
(defvar last-region-is-rect nil "Last region is rectangle or not.")
(defvar last-region-use-cua nil "Last region use CUA mode or not.")

(require 'lrf-basic-config)		;基本操作的配置，不依赖任何扩展的lisp库
(require 'lrf-exbasic-config)	;基本配置，需要第三方扩展的lisp库
(require 'lrf-theme-config)		;主题

;; 非常方便查看emacs帮助的插件
(require 'describe-symbol)
(require 'find-symbol)
(load "describe-find-symbol-settings")

;; 加载自定义的快捷键设置
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
