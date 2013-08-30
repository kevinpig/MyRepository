;; -*- Emacs-Lisp -*-

;; Time-stamp: <Kevin181489 2010-09-19 10:46:09>

(require 'template)

(template-initialize)
(setq template-default-directories (list (concat my-emacs-path "/templates/")))
(defvar last-template nil "最近使用的模版文件")
(defun my-template-expand-template (template)
  "展开template的模版文件"
  (interactive
   (list
    (read-file-name
     (if last-template (format "请指定模版文件(缺省为%s): " last-template) "请指定模版文件: ")
     (concat my-emacs-path "templates") last-template t)))
  (template-expand-template template)
  (setq last-template template))



