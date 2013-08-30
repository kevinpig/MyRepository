;;; sqlite-dump.el --- view dump of sqlite database file

;; Copyright 2009, 2010 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 5
;; Keywords: data
;; EmacsWiki: SQLite
;; URL: http://user42.tuxfamily.org/sqlite-dump/index.html

;; sqlite-dump.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; sqlite-dump.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This spot of code runs the sqlite3 program to .dump a database file as
;; SQL text for viewing and editing.  Use `sqlite-dump' in `auto-mode-alist'
;; to automatically visit desired files this way.
;;
;; The `buffer-file-format' mechanism is used with a dump for decode and
;; running the SQL to re-write.  See the `sqlite-dump' docstring for
;; details.

;;; Install:

;; To make M-x sqlite-dump available, put sqlite-dump.el in one of your
;; `load-path' directories and the following in your .emacs
;;
;;     (autoload 'sqlite-dump "sqlite-dump" nil t)
;;
;; To have it automatically on for instance .sqlite files (as used by
;; various mozilla family browsers) then add
;;
;;     (modify-coding-system-alist 'file "\\.sqlite\\'" 'raw-text-unix)
;;     (add-to-list 'auto-mode-alist '("\\.sqlite\\'" . sqlite-dump))
;;
;; There's autoload tags below for the function, if you use
;; `update-file-autoloads' and friends.  The coding and auto-mode-alist are
;; not autoloaded set yet as it's not clear what extension is usual, or
;; whether a dump to visit is always what's wanted.

;;; History:

;; Version 1 - the first version
;; Version 2 - add sqlite 2.x
;; Version 3 - undo defadvice on unload-feature
;; Version 4 - better write-region-post-annotation-function
;; Version 5 - express dependency on 'advice

;;; Emacsen:

;; Designed for Emacs 21 up, works in XEmacs 21.

;;; Code:

;; for `ad-find-advice' macro when running uncompiled
;; (don't unload 'advice before our -unload-function)
(require 'advice)

;;-----------------------------------------------------------------------------
;; xemacs incompatibilities
(eval-and-compile
  (defalias 'sqlite-dump--make-temp-file
    (if (eval-when-compile (fboundp 'make-temp-file))
        'make-temp-file   ;; emacs
      ;; xemacs21
      (autoload 'mm-make-temp-file "mm-util") ;; from gnus
      'mm-make-temp-file))
  (defalias 'sqlite-dump--set-buffer-multibyte
    (if (eval-when-compile (fboundp 'set-buffer-multibyte))
        'set-buffer-multibyte  ;; emacs
      'identity)))             ;; not applicable in xemacs21


;;-----------------------------------------------------------------------------
;; mainly generic

(defmacro sqlite-dump--with-tempfile (&rest body)
  "Create a `tempfile' variable for use by the BODY forms.
An `unwind-protect' ensures the file is removed no matter what
BODY does."
  `(let ((tempfile (sqlite-dump--make-temp-file "sqlite-dump-el-" nil)))
     (unwind-protect
         (progn ,@body)
       (delete-file tempfile))))

(defmacro sqlite-dump--with-tempdbfile (&rest body)
  "Create a `tempdbfile' variable for use by the BODY forms.
An `unwind-protect' ensures it and any associated \"-journal\"
file is removed no matter what BODY does."
  `(let* ((tempdir    (sqlite-dump--make-temp-file "sqlite-dump-el-" t))
          (tempdbfile (expand-file-name "tempdb" tempdir)))
     (message "tempdir %S" tempdir)
     (unwind-protect
         (progn ,@body)
       (let ((default-directory tempdir))
         (mapc 'delete-file
               (delete "." (delete ".." (directory-files tempdir)))))
       (delete-directory tempdir))))

;; quieten byte compiler pre emacs23
(defvar write-region-post-annotation-function)

(defmacro sqlite-dump--without-post-kill (&rest body)
  "Evaluate BODY without post-annotation kill-buffer.
If `write-region-post-annotation-function' is set buffer-local to
`kill-buffer' then set it to nil for BODY, and restore by an
`unwind-protect' afterwards.

This is a workaround for a bug in Emacs 23.1 where
`write-region-post-annotation-function' is set to `kill-buffer',
meaning any writes done by an encode function kill the buffer
that the encode is supposed to be operating on, usually making it
go on to mangle the contents of an unrelated buffer."

  `(let* ((sqlite-dump--without-post-kill--bad
           (and (local-variable-p 'write-region-post-annotation-function
                                  (current-buffer))
                (eq write-region-post-annotation-function
                    'kill-buffer)))
          (sqlite-dump--without-post-kill--buffer (current-buffer)))
     (unwind-protect
         (progn
           (if sqlite-dump--without-post-kill--bad
               (setq write-region-post-annotation-function nil))
           ;; (message "buf  %S" sqlite-dump--without-post-kill--buffer)
           ;; (message " bad  %S" sqlite-dump--without-post-kill--bad)
           ;; (message " now  %S" write-region-post-annotation-function)
           ,@body)
       (and sqlite-dump--without-post-kill--bad
            (buffer-live-p sqlite-dump--without-post-kill--buffer)
            (with-current-buffer sqlite-dump--without-post-kill--buffer
              (set (make-local-variable 'write-region-post-annotation-function)
                   'kill-buffer))))))

(defun sqlite-dump-display-buffer-other-window (buffer)
  "Display BUFFER in another window.
For a new window `shrink-window-if-larger-than-buffer' is used to
set its size.  If BUFFER is already in another window then its
size is left alone."
  (save-current-buffer
    (save-selected-window
      (let ((existing-window (get-buffer-window buffer)))
        (condition-case nil
            ;; emacs two args
            (switch-to-buffer-other-window buffer t) ;; no-record
          (error
           ;; xemacs one arg
           (switch-to-buffer-other-window buffer)))
        (if (not existing-window)
            (shrink-window-if-larger-than-buffer
             (get-buffer-window buffer)))))))


;;-----------------------------------------------------------------------------

;; Error messages from sqlite3, eg.
;;     Error: near line 1: near "fjsdk": syntax error
;;
;; There's no filename in the messages, even when ".read foo.sql" from a
;; file.  A couple of hacks below get the right buffer for
;; `sqlite-dump-encode'.
;;
(eval-after-load "compile"
  '(let ((error-elem '(sqlite-dump--sqlite3
                       "^Error: near line \\([0-9]+\\): " nil 1))
         (file-elem  '(sqlite-dump--filename
                       "^\\(### sqlite-dump-encode input:\\)$" 1)))
     (cond
      ((eval-when-compile (boundp 'compilation-error-regexp-systems-list))
       ;; xemacs21
       (add-to-list 'compilation-error-regexp-alist-alist
                    (list (car error-elem)
                          '("^\\(Error: near line \\)\\([0-9]+\\): " 1 2)))
       (compilation-build-compilation-error-regexp-alist))

      ((eval-when-compile (boundp 'compilation-error-regexp-alist-alist))
       ;; emacs22 up
       (add-to-list 'compilation-error-regexp-alist-alist file-elem)
       (add-to-list 'compilation-error-regexp-alist       (car file-elem))
       (add-to-list 'compilation-error-regexp-alist-alist error-elem)
       (add-to-list 'compilation-error-regexp-alist       (car error-elem)))

      (t
       ;; emacs21
       (add-to-list 'compilation-error-regexp-alist (cdr error-elem))
       (add-to-list 'compilation-file-regexp-alist (cdr file-elem))))))

(defvar sqlite-dump-originating-buffer nil
  "Originating SQL text buffer for *sqlite-dump-errors*.
This has a buffer-local value in the *sqlite-dump-errors* buffer.")
(make-variable-buffer-local 'sqlite-dump-originating-buffer)

(defadvice compilation-find-file (around sqlite-dump activate)
  "Use `sqlite-dump-originating-buffer' for sqlite errors."
  (if (and sqlite-dump-originating-buffer
           (member filename '("### sqlite-dump-encode input:"
                              "*unknown*" ;; emacs23
                              "Error: near line "))) ;; xemacs21 hack
      (setq ad-return-value sqlite-dump-originating-buffer)
    ad-do-it))

(defun sqlite-dump-unload-function ()
  (when (ad-find-advice 'compilation-find-file 'around 'sqlite-dump)
    (ad-remove-advice   'compilation-find-file 'around 'sqlite-dump)
    (ad-activate        'compilation-find-file))
  nil) ;; and do normal unload-feature actions too


;;-----------------------------------------------------------------------------

(add-to-list 'format-alist '(sqlite3-dump
                             "SQLite3 database file dump."
                             nil ;; no automatic decode
                             sqlite3-dump-decode
                             sqlite3-dump-encode
                             t     ;; encode modifies the region
                             nil)) ;; write removes from buffer-file-formats

(add-to-list 'format-alist '(sqlite2-dump
                             "SQLite2 database file dump."
                             nil ;; no automatic decode
                             sqlite2-dump-decode
                             sqlite2-dump-encode
                             t     ;; encode modifies the region
                             nil)) ;; write removes from buffer-file-formats

(defun sqlite2-dump-decode (beg end)
  "Run sqlite .dump on raw database bytes in the buffer.
This function is for use from `format-alist'."
  (sqlite-dump-decode "sqlite" 'iso-8859-1 beg end))
(defun sqlite3-dump-decode (beg end)
  "Run sqlite3 .dump on raw database bytes in the buffer.
This function is for use from `format-alist'."
  (sqlite-dump-decode "sqlite3" 'utf-8 beg end))

(defun sqlite2-dump-encode (beg end buffer)
  "Run sqlite on SQL statements in the current buffer.
This function is for use from `format-alist'."
  (sqlite-dump-encode "sqlite" 'iso-8859-1 beg end buffer))
(defun sqlite3-dump-encode (beg end buffer)
  "Run sqlite3 on SQL statements in the current buffer.
This function is for use from `format-alist'."
  (sqlite-dump-encode "sqlite3" 'utf-8 beg end buffer))

(defun sqlite-dump-decode (program coding beg end)
  "Run PROGRAM .dump on raw database bytes in the buffer.
The buffer should normally be unibyte as per a `raw-text-unix'
read, but anything that writes out unchanged is acceptable.  The
bytes are put through \"sqlite3 .dump\" to get SQL text and the
buffer is then switched to multibyte.  Error messages are shown
if sqlite3 can't be run or the database contents are invalid."

  (save-excursion
    (save-restriction
      (narrow-to-region beg end)

      (let (status)
        (sqlite-dump--with-tempfile
         (write-region (point-min) (point-max) tempfile nil 'quietly)
         (delete-region (point-min) (point-max))

         (let ((coding-system-for-read
                (if (memq coding (coding-system-list))
                    coding 'undecided))
               (default-directory       "/")
               (process-connection-type nil)) ;; pipe
           (setq status (call-process-region
                         (point-min) (point-max)
                         program
                         t       ;; delete old
                         t       ;; stdout+stderr this buffer
                         nil     ;; no redisplay
                         tempfile ".dump"))))

        (sqlite-dump--set-buffer-multibyte t)
        (unless (eq 0 status)
          (if (numberp status)
              (insert (format "\n\nexit %s" status))
            (insert "\n\n" status)))

        (point-max)))))

(defun sqlite-dump-encode (program coding beg end buffer)
  "Run sqlite3 on SQL statements in the current buffer.
The buffer text is put through PROGRAM to create a new database
file and its bytes replaces the text, switched to unibyte."

  (sqlite-dump--without-post-kill
   (save-excursion
     (save-restriction
       (narrow-to-region beg end)
       (let ((error-buffer (get-buffer-create "*sqlite-dump-errors*"))
             status)
         (with-current-buffer error-buffer
           (setq buffer-read-only nil)
           (fundamental-mode)
           (erase-buffer))
         (sqlite-dump--with-tempdbfile
          (let ((default-directory "/")
                (process-connection-type nil)) ;; pipe
            (setq status (call-process-region
                          (point-min) (point-max)
                          program
                          nil           ;; keep text
                          error-buffer  ;; stdout+stderr to errors
                          nil           ;; no redisplay
                          tempdbfile))) ;; new database

          (if (eq 0 status)
              (progn
                ;; successful, get new database bytes
                (sqlite-dump--set-buffer-multibyte nil)
                (let ((coding-system-for-read 'raw-text-unix))
                  (insert-file-contents-literally tempdbfile
                                                  nil     ;; no visit
                                                  nil nil ;; whole file
                                                  t))     ;; replace
                (delete-windows-on error-buffer)
                (kill-buffer error-buffer)
                (point-max))

            ;; error, display messages
            (with-current-buffer error-buffer
              ;; emacs21 ignores the first two lines of a compilation-mode
              ;; buffer, so add in dummies
              (goto-char (point-min))
              (if (numberp status)
                  (insert (format "exit %s\n\n" status))
                (insert (format "%s\n\n" status)))

              ;; matched by `sqlite-dump--filename' compile pattern above
              (insert "### sqlite-dump-encode input:\n")

              (goto-char (point-min))
              (sqlite-dump-display-buffer-other-window (current-buffer))
              (compilation-mode)
              (setq sqlite-dump-originating-buffer buffer)

              (error "sqlite encode error, see *sqlite-dump-errors* buffer")))))))))


;;-----------------------------------------------------------------------------

(defconst sqlite2-dump-regexp
  "\\*\\* This file contains an SQLite 2\\.1 database \\*\\*\000")
(defconst sqlite3-dump-regexp
  "SQLite format 3\000")

;;;###autoload
(defun sqlite-dump ()
  "Decode an SQLite database file to SQL text.
The buffer should be raw bytes (`raw-text-unix' unibyte).

The SQL is formed by either

    sqlite .dump      -- SQLite 2 database
    sqlite3 .dump     -- SQLite 3 database

The transform uses the `buffer-file-format' mechanism so you can
edit the SQL and save to re-write the database.  A save replaces
the entire file which means it's not safe if programs are
currently accessing it.

Note that .dump tends to be quite forgiving of truncated or
corrupt database files.  This is good for viewing, but doesn't
tell you if some data loss may be occurring.

--------
For SQLite 3, the dump is utf-8 and is encoded/decoded as such
\(except in an old Emacs without utf-8).  It's possible to have
invalid encodings in a database, such as a binary BLOB without
the right flags set, so check that before blaming the dump for a
bad display.

The dump includes \"PRAGMA foreign_keys=OFF\" to foreign key
constraint checking.  This means the order of insert statements
doesn't matter, but also means any edits you make are not
checked, so be careful.  Of course the database might already
have violations, since constraint enforcement is only optional.
Key constraints off allows such a database to be saved.

As of SQLite 3.6.21 the dump doesn't include a pragma to preserve
the utf-16 flag in the database and you end up with a utf-8
database on saving.  This makes no difference to actual
operation, but may be undesirable if it was utf-16 to avoid
runtime conversions in a particular program.

--------
For SQLite 2, any non-ascii is assumed to be latin-1.  The
library is more or less agnostic to any unibyte superset of
ascii, so the actual characters in the database might be
something else.

There's a compile-time SQLITE_UTF8 option affecting string
lengths etc, but it's not communicated in the dump, and a given
database may or may not have been created under that option.
Treating the dump as latin-1 will at least give something to look
at.

--------
The SQLite home page is
  URL `http://www.sqlite.org'
The sqlite-dump.el home page is
  URL `http://user42.tuxfamily.org/sqlite-dump/index.html'"

  (interactive)
  (unless (or (memq 'sqlite2-dump buffer-file-format)  ;; already decoded
              (memq 'sqlite3-dump buffer-file-format))
    (let* ((case-fold-search nil)
           (format (save-excursion
                     (goto-char (point-min))
                     (cond ((looking-at sqlite2-dump-regexp) 'sqlite2-dump)
                           ((looking-at sqlite3-dump-regexp) 'sqlite3-dump)
                           (t (error "Not an SQLite 2 or 3 database file"))))))
      (let ((inhibit-read-only t))
        (format-decode-buffer format))
      (sql-mode))))

(provide 'sqlite-dump)

;;; sqlite-dump.el ends here
