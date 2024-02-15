;;; backup-each-save.el --- backup each savepoint of a file

;; Copyright (C) 2004  Benjamin Rutt
;; Copyright (C) 2024  Abdelhak BOUGOUFFA

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;;         Abdelhak BOUGOUFFA
;; Version: 2.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Ever wish to go back to an older saved version of a file?  Then
;; this package is for you.  This package copies every file you save
;; in Emacs to a backup directory tree (which mirrors the tree
;; structure of the filesystem), with a timestamp suffix to make
;; multiple saves of the same file unique.  Never lose old saved
;; versions again.

;; To activate globally, place this file in your `load-path', and add
;; the following lines to your ~/.emacs file:
;;
;; (require 'backup-each-save)
;; (add-hook 'after-save-hook 'backup-each-save)

;; To activate only for individual files, add the require line as
;; above to your ~/.emacs, and place a local variables entry at the
;; end of your file containing the statement:
;;
;; eval: (add-hook (make-local-variable 'after-save-hook) 'backup-each-save)
;;
;; NOTE:  I would give a full example of how to do this here, but it
;; would then try to activate it for this file since it is a short
;; file and the docs would then be within the "end of the file" local
;; variables region.  :)

;; To filter out which files it backs up, use a custom function for
;; `backup-each-save-filter-function'.  For example, to filter out
;; the saving of gnus .newsrc.eld files, do:
;;
;; (defun backup-each-save-no-newsrc-eld (filename)
;;   (cond
;;    ((string= (file-name-nondirectory filename) ".newsrc.eld") nil)
;;    (t t)))
;; (setq backup-each-save-filter-function 'backup-each-save-no-newsrc-eld)

;;; ChangeLog
;; v1.0 -> v1.1:  added backup-each-save-filter-function
;; v1.1 -> v1.2:  1) added backup-each-save-size-limit
;;                2) fixed "Local Variables" docs, which was inadvertently
;;                   being activated
;; v1.2 -> v1.3:  fix for some emacsen not having `file-remote-p'
;; v1.3 -> v1.4:  added footer and autoload
;; v1.4 -> v2.0:  refactor, deprecate old Emacs
;; v2.0 -> v2.1:  1) more features and tweaks
;;                2) add `backup-each-save-cleanup' and `backup-each-save-auto-cleanup'
;;                3) add `backup-each-save-open-backup'

;;; Code:

(autoload 'cl-set-difference "cl-seq")

(defvar backup-each-save-directory (locate-user-emacs-file "backup-each-save/"))

(defvar backup-each-save-remote-files t
  "Whether to backup remote files at each save.

Defaults to nil.")

(defconst backup-each-save-time-format "%Y-%m-%d-%H-%M-%S"
  "Format given to `format-time-string' which is appended to the filename.")

(defconst backup-each-save-time-match-regexp "[[:digit:]]\\{4\\}\\(-[[:digit:]]\\{2\\}\\)\\{5\\}"
  "A regexp that matches `backup-each-save-time-format'.")

(defvar backup-each-save-filter-function #'identity
  "Function which should return non-nil if the file should be backed up.")

(defvar backup-each-save-size-limit 800000
  "Maximum size of a file (in bytes) that should be copied at each savepoint.

If a file is greater than this size, don't make a backup of it.
Setting this variable to nil disables backup suppressions based
on size.")

(defvar backup-each-save-cleanup-keep 20
  "Number of copies to keep for each file in `backup-each-save-cleanup'.")

(defvar backup-each-save-auto-cleanup nil
  "Automatically cleanup after making a backup.")

(defun backup-each-save ()
  "Perform a backup the `buffer-file-name' if needed."
  (let ((filename (buffer-file-name)))
    (when (and (or backup-each-save-remote-files (not (file-remote-p filename)))
               (funcall backup-each-save-filter-function filename)
               (or (not backup-each-save-size-limit) (<= (buffer-size) backup-each-save-size-limit)))
      (copy-file filename (backup-each-save-compute-location filename 'unique) t t t)
      (when backup-each-save-auto-cleanup (backup-each-save-cleanup filename)))))

(defun backup-each-save-compute-location (filename &optional unique)
  "Compute backup location for FILENAME.

When UNIQUE is provided, add a date after the file name."
  (let* ((localname (or (file-remote-p filename 'localname) filename))
         (method (or (file-remote-p filename 'method) "local"))
         (host (or (file-remote-p filename 'host) "localhost"))
         (user (or (file-remote-p filename 'user) user-real-login-name))
         (containing-dir (file-name-directory localname))
         (backup-dir (funcall #'file-name-concat backup-each-save-directory method host user containing-dir))
         (backup-basename (format "%s%s" (file-name-nondirectory localname) (if unique (concat "#" (format-time-string backup-each-save-time-format)) ""))))
    (when (not (file-exists-p backup-dir))
      (make-directory backup-dir t))
    (expand-file-name backup-basename backup-dir)))

(defun backup-each-save-backups-for-file (filename)
  "List of backups for FILENAME."
  (let* ((backup-filename (backup-each-save-compute-location filename))
         (backup-dir (file-name-directory backup-filename)))
    (directory-files backup-dir nil (concat "^" (regexp-quote (file-name-nondirectory backup-filename)) "#" backup-each-save-time-match-regexp "$"))))

;;;###autoload
(defun backup-each-save-cleanup (filename)
  "Cleanup backups of FILENAME, keeping `backup-each-save-cleanup-keep' copies."
  (interactive (list buffer-file-name))
  (if (not filename)
      (user-error "This buffer is not visiting a file")
    (let* ((backup-dir (file-name-directory (backup-each-save-compute-location filename)))
           (backup-files (backup-each-save-backups-for-file filename)))
      (dolist (file (cl-set-difference backup-files (last backup-files backup-each-save-cleanup-keep) :test #'string=))
        (let ((fname (expand-file-name file backup-dir)))
          (delete-file fname t))))))

;;;###autoload
(defun backup-each-save-open-backup (filename)
  "Open a backup of FILENAME or the current buffer."
  (interactive (list buffer-file-name))
  (if (not filename)
      (user-error "This buffer is not visiting a file")
    (let* ((current-major-mode major-mode)
           (backup-dir (file-name-directory (backup-each-save-compute-location filename)))
           (completion-extra-properties
            `(:annotation-function
              ,(lambda (bak) (format "   [%s bytes]" (file-attribute-size (file-attributes (expand-file-name bak backup-dir)))))))
           (backup-file (completing-read "Select file: " (backup-each-save-backups-for-file buffer-file-name))))
      (with-current-buffer (find-file (expand-file-name backup-file backup-dir))
        ;; Apply the same major mode as the original
        (funcall current-major-mode)))))

;;;###autoload
(define-minor-mode backup-each-save-mode
  "Automatically backup files after saving them."
  :init-value nil
  :lighter " Backup"
  :global t
  (if backup-each-save-mode
      (add-hook 'after-save-hook 'backup-each-save)
    (remove-hook 'after-save-hook 'backup-each-save)))


(provide 'backup-each-save)
;;; backup-each-save.el ends here
