;; -*- Emacs-Lisp -*-

;; Time-stamp <Kevin181489 2010-09-19 10:29:19>

(require 'pick-backup)

(global-set-key [f2] 'undo)
(global-set-key [f3] 'redo)
(global-set-key [f4] 'kill-this-buffer)
(global-set-key [f5] 'eshell)
(global-set-key [f6] 'dired-jump)
(global-set-key [f9] 'viper-mode)
(global-set-key [f11] 'calendar)

(apply-define-key
 global-map
 `(
   ("\e\e" viper-mode)					;定义ESC ESC切换到viper-mode
   ("RET" comment-indent-new-line)		;自动换行并注释
   ("C-c v v" pick-backup-and-view)		;查看备份版本
   ("C-c v d" pick-backup-and-ediff)	;比较备份版本的不同
   ("C-c v r" pick-backup-and-revert)	;恢复指定的备份版本
   
   ("C-o" other-window)					;切换到其他窗口，默认键不是很好用
   ("C-x c" copy-whole-buffer)
   ("C-x C" kill-whole-buffer)
   ;; 删除整个章节
   ("M-k" kill-whole-paragraph)
   ;; 删除从光标到章节尾的所有内容
   ("M-C-k" kill-paragraph)
   ;; 复制整个章节
   ("M-C" copy-whole-paragraph)
   ("C-x M-w" insert-cur-line)
   ;; 删除光标到行尾的整行
   ("M-K" kill-line)
   ("C-x M-W" insert-cur-sexp)
   ("C-M-w" copy-sentence)
   ;; 删除整行，不论光标在哪里
   ("C-k" kill-whole-line)
   ("M-W" which-copy)
   ("M-w" smart-copy)
   ("C-\\" delete-indentation)
   ("C-x M-M" mark-invisible-region)
   ("M-U" del-to-begin)
   ;; 转化大小写
   ("C-^" case-trans)
   ("C-w" backward-kill-word-or-kill-region)
   ("C-x S" mark-whole-sexp)
   ("C-x W" kill-sexp)
   ;; 复制整个表达式，即如果在光标在""字符上会复制""中的所有内容
   ("C-x w" copy-sexp)
   ("M-D" my-kill-word)
   ("C-x TAB" indent-whole-buffer)
   ("C-h" c-electric-backspace-kill)
   ("C-?" redo)))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (apply-define-key
   map
   `(("C-c D"  edebug-defun)
	 ("C-c C-d" eval-defun)
	 ("C-c B"  eval-buffer)
	 )))

;; smart compile key setting
(let ((list makefile-mode-map-list))
  (setq list (append list (list c-mode-base-map svn-status-mode-map sh-mode-map
								compilation-mode-map ruby-mode-map)))
  (dolist (map list)
    (define-key map (kbd "C-c C-m") 'make)
    (define-key map (kbd "C-c m") 'make-check)
    (define-key map (kbd "C-c M") 'make-clean)
    (define-key map (kbd "C-c C-c") 'compile-buffer)
    (define-key map (kbd "C-c r") 'run-program)
    (define-key map (kbd "C-c C-C") 'smart-compile)))
(dolist (map (list java-mode-map))
  (define-key map (kbd "C-c C-m") 'ant)
  (define-key map (kbd "C-c M") 'ant-clean)
  (define-key map (kbd "C-c m") 'ant-test))

;; 显示上一个编译错误
(global-set-key (kbd "M-p") 'previous-error)
(dolist (map makefile-mode-map-list)
  (define-key map (kbd "M-p") 'previous-error)
  (define-key map (kbd "M-n") 'next-error)
  (define-key map (kbd "C-c p") 'makefile-previous-dependency)
  (define-key map (kbd "C-c n") 'makefile-next-dependency))

(define-key compilation-mode-map (kbd "n") 'compilation-next-error)
(define-key compilation-mode-map (kbd "p") 'compilation-previous-error)

(dolist (map (list c-mode-base-map python-mode-map emacs-lisp-mode-map lisp-interaction-mode-map ))
  (apply-define-key
   map
   `(
	 ;; 复制整个函数，或者整个struct，class的定义
	 ("C-c f" copy-function)
	 ;; 删除整个函数，或者整个struct，class的定义
	 ("C-c F" kill-function)
	 ;; 缩进整个函数
	 ("C-c TAB" indent-function)
	 ;; 注释掉整个函数
	 ("C-c C" comment-function)
	 ("C-M-h" mark-function)
	 ("C-c c" commet)
	 ("C-c u" uncomment)
	 )))

(dolist (map (list c-mode-base-map python-mode-map emacs-lisp-mode-map sh-mode-map ))
  (apply-define-key
  map
  `(
	("C-c T" my-template-expand-template)
	("C-c C-t" template-expand-template)
	)))

(provide 'lrf-key-map)
