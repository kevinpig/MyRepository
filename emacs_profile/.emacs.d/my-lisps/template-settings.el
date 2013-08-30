;; -*- Emacs-Lisp -*-

;; Time-stamp: <Kevin181489 2010-09-19 10:46:09>

(require 'template)

(template-initialize)
(setq template-default-directories (list (concat my-emacs-path "/templates/")))
(defvar last-template nil "���ʹ�õ�ģ���ļ�")
(defun my-template-expand-template (template)
  "չ��template��ģ���ļ�"
  (interactive
   (list
    (read-file-name
     (if last-template (format "��ָ��ģ���ļ�(ȱʡΪ%s): " last-template) "��ָ��ģ���ļ�: ")
     (concat my-emacs-path "templates") last-template t)))
  (template-expand-template template)
  (setq last-template template))



