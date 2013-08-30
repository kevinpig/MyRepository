(require 'sh-script)

(defvar sh-mode-map-key-pairs
  `(("<" self-insert-command)
    ("C-c M-c" sh-case)
    ("C-c C-c" comment)
    ("C-c g" bashdb))
  "*Key pairs for `sh-mode'.")

(apply-map-define-keys 'sh-mode-map)

(font-lock-add-keywords 'sh-mode '(("\\<\\(local\\|let\\)\\>" . font-lock-keyword-face)))

(provide 'shell-setting)
