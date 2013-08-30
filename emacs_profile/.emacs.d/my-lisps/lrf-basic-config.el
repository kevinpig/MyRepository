;;�������
;;ȥ��������
;(tool-bar-mode nil)
; make whitespace-mode use just basic coloring
(setq whitespace-style (quote
			( spaces tabs newline space-mark tab-mark newline-mark)))

;;Ĭ��ʹ��ibuffer
(defalias 'list-buffers 'ibuffer)

;;��������
;;emacs����һ���Զ����湦�ܣ�Ĭ����~/.emacs.d/auto-save-list�����ǳ����ã�������û�иĶ���������Բμ�Sams teach yourself emacs in 24hours(�Ҽ��Ϊsams24)
;;���ð汾���ƣ������Ա��ݶ��
(setq version-control t)
;;������ԭʼ�İ汾���Σ��ǵ�һ�α༭ǰ���ĵ����͵ڶ��α༭ǰ���ĵ�
(setq kept-old-versions 2)
;;�������µİ汾��Σ����ͬ��
(setq kept-new-versions 5)
;;ɾ������������7�а汾�İ汾
(setq delete-old-versions t)
;;���ñ����ļ���·��
(setq backup-directory-alist '(("." . "~/.emacs.tmp")))
;;�������÷�����ֱ�ӿ���
(setq backup-by-copying t)

;;ʱ�������(time-stamp)���趨�ĵ��ϴα������Ϣ
;;ֻҪ��������ĵ�����Time-stamp:�����ã��ͻ��Զ�����ʱ���
;;����time-stamp
(setq time-stamp-active t)
;;ȥ��time-stamp�ľ��棿
(setq time-stamp-warn-inactive t)
;;����time-stamp�ĸ�ʽ�������µĸ�ʽ���õ�һ�����ӣ�<Kevin181489 2009-01-03 12:00:00>
(setq time-stamp-format "Kevin181489 %04y-%02m-%02d %02H:%02M:%02S")
;;���޸�ʱ�����ӵ������ļ��Ķ����
(add-hook 'write-file-hooks 'time-stamp)
;;Ĭ��ʹ��Vi-Mode�������Ͳ������޸��ļ���
(setq viper-mode t)
(require 'viper)
;;ʱ����ʾ����
;;����ʱ����ʾ���ã���minibuffer������Ǹ����ϣ����˽�ʲô���ţ�
(display-time-mode 1)
;;ʱ��ʹ��24Сʱ��
(setq display-time-24hr-format t)
;;ʱ����ʾ�������ں;���ʱ��
(setq display-time-day-and-date t)
;;ʱ�����Ա������ʼ�����
(setq display-time-use-mail-icon t)
;;ʱ��ı仯Ƶ�ʣ���λ�������ţ�
(setq display-time-interval 10)

;;����minibuffer��������Ĭ�����ð�
(minibuffer-electric-default-mode 1)
;;���ò��ֲ�ȫ���ܣ�������M-x q r r�൱��M-x query-replace-regexp
;; (partial-completion-mode 1)
;;��minibuffer�������Զ���ȫ�����ͱ���
(icomplete-mode 1)
;;���е�������y/n��ʽ������yes/no��ʽ���е�����ֻ������һ����ĸ
(fset 'yes-or-no-p 'y-or-n-p)
;;����minibuffer���ɱ仯���С��ָ��ȣ�
(setq resize-mini-windows t)
;;��Ѱ��һ��ͬ�����ļ����Զ��������Ǹ��ļ���
(setq uniquify-buffer-name-style 'forward)
;;��emacs��man�ĵ�ʱ��ʹ�õ�ǰbuffer
(setq Man-notify-method 'pushy)
;;����Զ��ܿ�ָ�룬�統�������ʱ��ָ�뵽������λ�ã�����е㵲ס������
(mouse-avoidance-mode 'animate)
;;�����Զ���ͼƬ����wiki����
(auto-image-file-mode)
;;���Բ���ѹ���ĵ�
(auto-compression-mode 1)
;;��minibuffer���������ʾ�к�
(column-number-mode t)
;;��ʾĬ�ϵ��ĵ��Ŀ�ȣ��������Ƚ������
(setq default-fill-column 120)
;;����tabΪ4���ո�Ŀ��
(setq default-tab-width 4)
;;ָ�벻Ҫ�����ҵ��۾�����
(blink-cursor-mode -1)
(transient-mark-mode 1)
;;��ָ�뵽һ������ʱ���Զ���ʾ��ƥ�����һ������
(show-paren-mode 1)
;; �﷨����
(global-font-lock-mode t)

;���ù������
;;(mouse-wheel-mode t)
;;ȥ�����˵ľ�������
(setq visible-bell nil)
;;����ҳ��ʱ�Ƚ��������Ҫ��ҳ�Ĺ���
(setq scroll-step 1
	  scroll-margin 3
	  scroll-conservatively 10000)
;;ȥ��Emacs��gnus����ʱ����������
(setq inhibit-startup-message t)
;; (setq gnus-inhibit-startup-message t)
;;��ָ���Ƶ���һ�У���Ҫ������һ�У�d
(setq next-line-add-newlines nil)

;;���ĵ�����Զ�����հ�һ�У�����ĳЩϵͳ�����ļ�����Ҫ������
(setq require-final-newline t)
(setq track-eol t)
;;ʹ��C-kɾ��ָ�뵽����ĩ�����ж���
(setq-default kill-whole-line t)
;;�趨ɾ�������¼Ϊ200�����Է����Ժ����޻ָ�
(setq kill-ring-max 200)
;;����ʹ�ò��Һ����ͱ�����Ѱ�ҷ�Χ
(setq apropos-do-all t)
;;����aspell������ΪEmacs��ƴд����ѧ
(setq-default ispell-program-name "aspell")
;;ʹ��narrow����ʱ��һ������
(put 'narrow-to-region 'disabled nil)
;;����Emacs�Զ�����Ϊ��������(���¸�һ��)
;;(split-window-vertically)
;;�ı�emacs�������ı���,%f��ʾ�ļ�ȫ��,%m��ʾ��ǰ��mode
(setq frame-title-format "%f@%m")
;;����emacs���ⲿ���������ճ��
(setq x-select-enable-clipboard t)
;;��buffer��ǰ����ʾ�ļ����к�
(global-linum-mode 1)
;;����linenumĬ�ϵĸ�ʽ
(setq linum-format "%4d ")

;;����cuaģʽ������ʹ��win32Ĭ�ϵ�cut��copy��paste
(cua-mode 1)
;;����C-z��
(setq cua-remap-control-z nil)
;;��׼win32��Ϊ��copy��ɾ��ԭ��������
(setq cua-keep-region-after-copy t)
(setq-default line-spacing 2)       ;; Add 1 pixel between lines
;;����һ��������ļ��Ĳ˵�
(recentf-mode)

(defvar mswin (equal window-system 'w32) "Non-nil means windows system.")
;; �������ŵ��ұ���
(if mswin
    ;; TODO: windows�����û�й������Ļ�describeϵ������
    ;; ���е�ʱ���е�����
    (customize-set-variable 'scroll-bar-mode 'right)
  (customize-set-variable 'scroll-bar-mode nil))

;; Emacs�Ҳ������ʵ�ģʽʱ��ȱʡʹ��text-mode
(setq default-major-mode 'text-mode)
(setq-default indent-tabs-mode nil)

(require 'pc-select)
(require 'lrf-font)
(require 'lrf-ido-config)

(provide 'lrf-basic-config)
