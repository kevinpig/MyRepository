;; -*- Emacs-Lisp -*-
(if is-before-emacs-21
    (progn
      (load "ido-for-21")
      (setq read-buffer-function 'ido-read-buffer))
  (require 'ido)
  (ido-everywhere t)
  (setq ido-define-mode-map-hook 'ido-setup-hook))

(ido-mode t)
(global-set-key (kbd "C-x C-f") 'ido-find-file)

(setq ido-enable-flex-matching t)                   ;Ä£ºýÆ¥Åä

(provide 'lrf-ido-config)
