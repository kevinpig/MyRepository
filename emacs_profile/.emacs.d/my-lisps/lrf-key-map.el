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
   ("\e\e" viper-mode)					;����ESC ESC�л���viper-mode
   ("RET" comment-indent-new-line)		;�Զ����в�ע��
   ("C-c v v" pick-backup-and-view)		;�鿴���ݰ汾
   ("C-c v d" pick-backup-and-ediff)	;�Ƚϱ��ݰ汾�Ĳ�ͬ
   ("C-c v r" pick-backup-and-revert)	;�ָ�ָ���ı��ݰ汾
   
   ("C-o" other-window)					;�л����������ڣ�Ĭ�ϼ����Ǻܺ���
   ("C-x c" copy-whole-buffer)
   ("C-x C" kill-whole-buffer)
   ;; ɾ�������½�
   ("M-k" kill-whole-paragraph)
   ;; ɾ���ӹ�굽�½�β����������
   ("M-C-k" kill-paragraph)
   ;; ���������½�
   ("M-C" copy-whole-paragraph)
   ("C-x M-w" insert-cur-line)
   ;; ɾ����굽��β������
   ("M-K" kill-line)
   ("C-x M-W" insert-cur-sexp)
   ("C-M-w" copy-sentence)
   ;; ɾ�����У����۹��������
   ("C-k" kill-whole-line)
   ("M-W" which-copy)
   ("M-w" smart-copy)
   ("C-\\" delete-indentation)
   ("C-x M-M" mark-invisible-region)
   ("M-U" del-to-begin)
   ;; ת����Сд
   ("C-^" case-trans)
   ("C-w" backward-kill-word-or-kill-region)
   ("C-x S" mark-whole-sexp)
   ("C-x W" kill-sexp)
   ;; �����������ʽ��������ڹ����""�ַ��ϻḴ��""�е���������
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

;; ��ʾ��һ���������
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
	 ;; ����������������������struct��class�Ķ���
	 ("C-c f" copy-function)
	 ;; ɾ��������������������struct��class�Ķ���
	 ("C-c F" kill-function)
	 ;; ������������
	 ("C-c TAB" indent-function)
	 ;; ע�͵���������
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
