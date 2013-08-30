;; -*- Emacs-Lisp -*-

;; Time-stamp: <Kevin181489 2010-11-19 08:38:27>

(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)
(require 'auto-complete-c)
(require 'auto-complete-etags)
(require 'auto-complete-extension)
(require 'auto-complete-octave)
(require 'auto-complete-verilog)
(require 'ac-anything)
(require 'rcodetools)
(require 'auto-complete+)
(require 'javascript-mode "javascript")
(require 'js2-mode)
(require 'css-mode)

(global-auto-complete-mode t)
(setq ac-auto-start 2)

(define-key ac-complete-mode-map (kbd "<return>") 'ac-complete)
(define-key ac-complete-mode-map (kbd "M-j")      'ac-complete)
(define-key ac-complete-mode-map (kbd "M-n")      'ac-next)
(define-key ac-complete-mode-map (kbd "M-p")      'ac-previous)

(setq ac-dwim t)
(setq ac-candidate-menu-height 20)
(setq ac-candidate-max ac-candidate-menu-height)

(set-default 'ac-sources
             '(ac-source-semantic
               ac-source-yasnippet
               ac-source-abbrev
               ac-source-words-in-buffer
               ac-source-words-in-all-buffer
               ac-source-imenu
               ac-source-files-in-current-dir
               ac-source-filename))
(setq ac-modes ac+-modes)

(setq
 ac-trigger-commands
 '(self-insert-command
   autopair-insert-or-skip-quote
   autopair-backspace
   c-electric-backspace
   c-electric-backspace-kill))

(defun ac-settings-4-lisp ()
  "Auto complete settings for lisp mode."
  (setq ac-omni-completion-sources '(("\\<require\s+'" ac-source-emacs-lisp-features)
                                     ("\\<load\s+\"" ac-source-emacs-lisp-features)))
  (ac+-apply-source-elisp-faces)
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-symbols
          ;; ac-source-semantic
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-imenu
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-java ()
  (setq ac-omni-completion-sources (list (cons "\\." '(ac-source-semantic))
                                         (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-c ()
  (setq ac-omni-completion-sources (list (cons "\\." '(ac-source-semantic))
                                         (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-c-keywords
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-cpp ()
            (setq ac-omni-completion-sources
                  (list (cons "\\." '(ac-source-semantic))
                        (cons "->" '(ac-source-semantic))))
            (setq ac-sources
                  '(ac-source-yasnippet
                    ac-source-c++-keywords
                    ac-source-abbrev
                    ac-source-words-in-buffer
                    ac-source-words-in-all-buffer
                    ac-source-files-in-current-dir
                    ac-source-filename)))

(defun ac-settings-4-text ()
            (setq ac-sources
                  '(;;ac-source-semantic
                    ac-source-yasnippet
                    ac-source-abbrev
                    ac-source-words-in-buffer
                    ac-source-words-in-all-buffer
                    ac-source-imenu)))

(defun ac-settings-4-eshell ()
            (setq ac-sources
                  '(;;ac-source-semantic
                    ac-source-yasnippet
                    ac-source-abbrev
                    ac-source-words-in-buffer
                    ac-source-words-in-all-buffer
                    ac-source-files-in-current-dir
                    ac-source-filename
                    ac-source-symbols
                    ac-source-imenu)))

(defun ac-settings-4-ruby ()
            (setq ac-omni-completion-sources
                  (list (cons "\\." '(ac-source-rcodetools))
                        (cons "::" '(ac-source-rcodetools)))))

(defun ac-settings-4-html ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

(defvar ac-mpc-source
  '(ac-source-mpc-keywords))

(ac-define-dictionary-source
 ac-source-mpc-keywords
 '("project" "workspace" "Source_Files" "IDL_Files" "Header_Files"
   "Template_Files" " Resource_Files" "Pkgconfig_Files" "exclude"
   "exename" "staticname" "dllout" "exeout" "install" "libs" "lit_libs"
   "pure_libs" "specific" "macros" "after" "libout" "libpaths" "includes"
   "sharedname" "dynamicflags" "idlflags"
 ))

(defun ac-mpc-setup()
  (setq ac-sources (append ac-mpc-source ac-sources)))

(defun ac-setting-4-mpc()
  (setq ac-sources
		'(
		  ac-source-mpc-keywords
		  ac-source-words-in-buffer
		  ac-source-files-in-current-dir
		  )))

(dolist (hook (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook
                    'svn-log-edit-mode))
  (add-hook hook 'ac-settings-4-lisp))

(add-hook 'java-mode-hook   'ac-settings-4-java)
(add-hook 'c-mode-hook      'ac-settings-4-c)
(add-hook 'c++-mode-hook    'ac-settings-4-cpp)
(add-hook 'text-mode-hook   'ac-settings-4-text)
(add-hook 'eshell-mode-hook 'ac-settings-4-eshell)
(add-hook 'ruby-mode-hook   'ac-settings-4-ruby)
(add-hook 'html-mode-hook   'ac-settings-4-html)
(add-hook 'mpc-mode-hook 'ac-setting-4-mpc )
;; (dolist (mode ac-modes)
;;   (let ((mode-name (symbol-name mode)))
;;     (when (and (intern-soft mode-name) (intern-soft (concat mode-name "-map")))
;;     (define-key (symbol-value (concat-name mode-name "-map")) (kbd "C-c a") 'ac-start))))

(provide 'auto-complete-settings)
