;; -*- Emacs-Lisp -*-

;; Time-stamp: <Kevin181489 2010-11-18 16:45:58>

;; cedet1.0pre6 is conflict with which-func
;; after require cedet, which-func cann't work
(require 'dframe)
(require 'eieio)
(require 'cedet)
(require 'semantic-ia)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

(semantic-load-enable-excessive-code-helpers)
(semantic-load-enable-semantic-debugging-helpers)
;;���mode���semantic�������˵������ú�ɫ���»��߱�ʶ���������ѿ�
(setq semantic-show-unmatched-syntax-mode nil) 

;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

;; TODO: �������Բ�������ȡ��`senator-prefix-key'��prefix command
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (make-local-variable 'senator-prefix-key)
             (setq senator-prefix-key nil)) t)
;; ����semantic�ļ�����Χ
(setq semanticdb-project-roots (list "/"))
(require 'cc-mode)
(dolist (map (list c-mode-base-map emacs-lisp-mode-map))
  ;;�����������������������ĵط�
  (define-key map (kbd "C-c j") 'semantic-ia-fast-jump)
;;  (define-key map (kbd "C-c j") 'semantic-complete-jump-local)
  ;;��������Ҫ���ҵķ���
  (define-key map (kbd "C-c r") 'semantic-symref-symbol)
  ;;���ҹ�����ڴ��ķ���
  (define-key map (kbd "C-c R") 'semantic-symref)
  (define-key map (kbd "C-c n") 'senator-next-tag)
  (define-key map (kbd "C-c p") 'senator-previous-tag))

(defun my-c-mode-common-hook ()
  ;; ��ͷ�ļ���cpp�ļ�֮���л�
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  ;;�г��ļ��еķ������б�
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; �������ռ�ù����CPU
(setq-default semantic-idle-scheduler-idle-time 432000)
;; ����Ĵ������������
;; ��ƽʱ����Ŀ��h��cpp��������ͬһ��Ŀ¼�У������õ�����Ŀ�о���ʹ�õ�ͷ�ļ�Ŀ¼
(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))

;; ����windowsϵͳ��ͷ�ļ���Ŀ¼
(defconst cedet-win32-include-dirs
  (list "C:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/include"
		"c:/program files/microsoft visual studio .net 2003/vc7/PlatformSDK/include"
		"c:/program files/microsoft visual studio .net 2003/sdk/v1.1/include"
		))
(require 'semantic-c nil 'noerror)

(let ((include-dirs cedet-user-include-dirs))
  (when (eq system-type 'windows-nt)
    (setq include-dirs (append include-dirs cedet-win32-include-dirs)))
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c++-mode)
          (semantic-add-system-include dir 'c-mode))
        include-dirs))

;; F12 �������
(global-set-key [f12] 'semantic-ia-fast-jump)
;; �����ϴε�λ��
(global-set-key [S-f12]
                (lambda ()
                  (interactive)
                  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
                      (error "Semantic Bookmark ring is currently empty"))
                  (let* ((ring (oref semantic-mru-bookmark-ring ring))
                         (alist (semantic-mrub-ring-to-assoc-list ring))
                         (first (cdr (car alist))))
                    (if (semantic-equivalent-tag-p (oref first tag)
                                                   (semantic-current-tag))
                        (setq first (cdr (car (cdr alist)))))
                    (semantic-mrub-switch-tags first))))

;; ecb
(require 'ecb)
(setq ecb-tip-of-the-day nil)
(defun ecb()
  "start ecb"
  (interactive)
  (ecb-activate)
  (ecb-layout-switch "left9"))

;;;; �����ڼ��л�
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
