;;; ice-mode.el  --- Slice mode for emacs

;; Author: Kevin <liuruifei@gmail.com>
;; Maintainer: Kevin
;; Keywords: language faces mpc

(require 'font-lock)

(defvar ice-mode-hook nil)

(defvar ice-mode-map
  (let ((ice-mode-map (make-sparse-keymap)))
    (define-key mpc-mode-map '[(control j)]   'newline-and-indent )
    mpc-mode-map)
  "Keymap for Ice major mode"  )

(defun ice-indent-line()
  "Indent current line as ice directives"
  (interactive)
  (beginning-of-line)

  (if (bobp)
      (indent-line-to 0) ; if we are at start of file, zero Indent
    (let ((not-found-hint t) cur-indent (close-brace nil))
      (save-excursion
        (if (looking-at ".*}")
            (setq close-brace t))
        (while not-found-hint ;
          (forward-line -1)
          (if (looking-at ".*{")
              (progn
                (setq cur-indent (+ (current-indentation) tab-width))
                (setq not-found-hint nil))
            (if (looking-at ".*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (if (< cur-indent 0)
                      (setq cur-indent 0))
                  (setq not-found-hint nil))
              (if (bobp)
                  (setq not-found-hint nil))))))
      (if close-brace
          (setq cur-indent (- cur-indent tab-width)))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0))))
  )

(defvar slice-keywords
  `("bool", "enum", "implement", "module", "string", "byte", "exception"
	"int", "nonmutating", "struct", "class", "extends", "interface",
	"Object", "throws", "const", "false", "local", "out", "true",
	"dictionary", "float", "LocalObject", "sequence", "void",
	"double", "idempotent", "long", "short")
  "Slice language keywords")

(defvar slice-keywords-regexp
  (regexp-opt slice-keywords 'words))

(setq slice-keywords nil)
(setq ice-font-lock-keywords
	  `(
		(, slice-keywords-regexp . font-lock-keyword-face)
		;; ("^#.+[\n]" 1 font-lock-type-face  )
		;; ("\\<struct[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" 1 font-lock-type-face)
		))

;;Create a syntax table .
(defvar ice-mode-syntax-table nil
  "syntax table used in ice mode")
(setq ice-mode-syntax-table (make-syntax-table))
(modify-syntax-entry ?_ "w" ice-mode-syntax-table)   ; underscore is a valid part of word
(modify-syntax-entry ?- "w" ice-mode-syntax-table)   ; hyphend is a valid part of word
(modify-syntax-entry ?\/ ". 124b" ice-mode-syntax-table)  ; c-style comments
(modify-syntax-entry ?\n "> b" ice-mode-syntax-table) ;c-style comments
(modify-syntax-entry ?* ". 23" ice-mode-syntax-table ) ;c++-style comment

(defvar ice-mode-abbrev-table nil
  "Abbrev table in use in ice-mode buffers")
(define-abbrev-table 'ice-mode-abbrev-table nil)


;;;###autoload
(defun ice-mode()
  "A major-mode to edit slice files."

  (interactive)
  (kill-all-local-variables)
  (use-local-map ice-mode-map)

  (make-local-variable 'comment-start)
  (setq comment-start "//")
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'tab-width)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'indent-line-function)

  (setq font-lock-defaults `(mpc-font-lock-keywords nil t))
  (setq indent-line-function 'ice-indent-line)
  (set-syntax-table ice-mode-syntax-table)

  (setq major-mode 'ice-mode)
  (setq mode-name "SLICE")
  (setq local-abbrev-table ice-mode-abbrev-table)
  (setq font-lock-defaults `(ice-font-lock-keywords))
  (setq indents-tab-mode nil)
  (run-hooks 'ice-mode-hook)
  )

(add-to-list 'auto-mode-alist '("\\.ice\\'" . ice-mode ))

(provide 'ice-mode)

;;; ice-mode.el ends here
