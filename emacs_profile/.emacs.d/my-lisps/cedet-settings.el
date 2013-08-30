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
;;这个mode会把semantic解析不了的内容用红色的下划线标识出来，很难看
(setq semantic-show-unmatched-syntax-mode nil) 

;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)

;; TODO: 怎样可以不用这样取消`senator-prefix-key'的prefix command
(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
             (make-local-variable 'senator-prefix-key)
             (setq senator-prefix-key nil)) t)
;; 配置semantic的检索范围
(setq semanticdb-project-roots (list "/"))
(require 'cc-mode)
(dolist (map (list c-mode-base-map emacs-lisp-mode-map))
  ;;快速跳动到函数或变量定义的地方
  (define-key map (kbd "C-c j") 'semantic-ia-fast-jump)
;;  (define-key map (kbd "C-c j") 'semantic-complete-jump-local)
  ;;请求输入要查找的符号
  (define-key map (kbd "C-c r") 'semantic-symref-symbol)
  ;;查找光标所在处的符号
  (define-key map (kbd "C-c R") 'semantic-symref)
  (define-key map (kbd "C-c n") 'senator-next-tag)
  (define-key map (kbd "C-c p") 'senator-previous-tag))

(defun my-c-mode-common-hook ()
  ;; 在头文件和cpp文件之间切换
  (define-key c-mode-base-map (kbd "M-o") 'eassist-switch-h-cpp)
  ;;列出文件中的方法的列表
  (define-key c-mode-base-map (kbd "M-m") 'eassist-list-methods))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; 避免分析占用过多的CPU
(setq-default semantic-idle-scheduler-idle-time 432000)
;; 下面的代码好像不起作用
;; 在平时的项目中h和cpp经常不在同一个目录中，这里用到了项目中经常使用的头文件目录
(defconst cedet-user-include-dirs
  (list ".." "../include" "../inc" "../common" "../public"
        "../.." "../../include" "../../inc" "../../common" "../../public"))

;; 定义windows系统的头文件的目录
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

;; F12 代码快速
(global-set-key [f12] 'semantic-ia-fast-jump)
;; 跳回上次的位置
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

;;;; 各窗口间切换
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
