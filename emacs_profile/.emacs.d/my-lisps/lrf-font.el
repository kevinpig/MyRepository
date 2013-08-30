;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; �������� ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar emacs-english-font "Courier New"
  "The font name of English.")
(defvar emacs-cjk-font "Droid Sans Fallback"
  "The font name of CJK.")
(defvar emacs-font-size 12
  "The default font size.")

;; Ӣ������
(set-frame-font (format "%s-%s" (eval emacs-english-font) (eval emacs-font-size)))
;; ��������
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'unicode (eval emacs-cjk-font))

(provide 'lrf-font)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; file ends here
;;;
