(require 'color-theme)
(color-theme-initialize)
(color-theme-calm-forest)

(require 'ahei-face)

;; dired+
(require 'dired+)
(custom-set-faces '(diredp-display-msg
                    ((((type tty)) :foreground "blue")
                     (t :foreground "cornflower blue"))))
(custom-set-faces '(diredp-date-time
                    ((((type tty)) :foreground "yellow")
                     (t :foreground "goldenrod1"))))
(custom-set-faces '(diredp-dir-heading
                    ((((type tty)) :background "yellow" :foreground "blue")
                     (t :background "Pink" :foreground "DarkOrchid1"))))
(setq diredp-ignored-file-name 'green-face)
(setq diredp-file-name 'darkred-face)
(setq diredp-file-suffix 'magenta-face)
(setq diredp-exec-priv 'darkyellow-face)
(setq diredp-other-priv 'darkyellow-face)
(setq diredp-no-priv 'darkyellow-face)
(setq diredp-write-priv 'darkyellow-face)
(setq diredp-read-priv 'darkyellow-face)
(setq diredp-link-priv 'darkyellow-face)
(setq diredp-symlink 'darkyellow-face)
(setq diredp-rare-priv 'darkyellow-face)
(setq diredp-dir-priv 'darkyellow-face)
(setq diredp-compressed-file-suffix 'darkyellow-face)

;; (require 'color-theme-ahei)
;; (require 'face-settings)

(provide 'lrf-theme-config)
