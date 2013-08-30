
(require 'tempo)
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode)
		  (eq major-mode 'c++-mode))
	  (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
(setq-default doxymacs-doxygen-style "JavaDoc")

(require 'doc-mode)
(add-hook 'c-mode-common-hook 'doc-mode)

(provide 'doxygen_setting)
