(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
								   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-hook 'python-mode-hook
		  (lambda()
			(set-variable 'py-indent-offset 4)
            (setq default-tab-width 4)
			(setq indent-tabs-mode nil)
			;;(auto-complete-mode)
			(company-mode)	
			))

;;if mswin
;;	(progn
;;	  (autoload 'pymacs-apply "pymacs")
;;	  (autoload 'pymacs-call "pymacs")
;;	  (autoload 'pymacs-eval "pymacs" nil t)
;;	  (autoload 'pymacs-exec "pymacs" nil t)
;;	  (autoload 'pymacs-load "pymacs" nil t)

;;	  (pymacs-load "ropemacs" "rope-")
;;	  (setq ropemacs-enable-autoimport t)))

;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

(require 'pydb)
(autoload 'pydb "pydb" "Python Debugger mode via GUD and pydb" t)

;; pdb setup, note the python version
(setq pdb-path 'c:/Python25/Lib/pdb.py
	  gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
							(file-name-nondirectory buffer-file-name)))))
                            
(provide 'python-setting)
