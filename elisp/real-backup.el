;;; real-backup.el --- backup each savepoint of a file

;; Copyright (C) 2004  Benjamin Rutt
;; Copyright (C) 2024  Abdelhak BOUGOUFFA

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;;         Abdelhak BOUGOUFFA
;; Version: 3.1

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
;; (require 'real-backup)
;; (add-hook 'after-save-hook 'real-backup)

;; To activate only for individual files, add the require line as
;; above to your ~/.emacs, and place a local variables entry at the
;; end of your file containing the statement:
;;
;; eval: (add-hook (make-local-variable 'after-save-hook) 'real-backup)
;;
;; NOTE:  I would give a full example of how to do this here, but it
;; would then try to activate it for this file since it is a short
;; file and the docs would then be within the "end of the file" local
;; variables region.  :)

;; To filter out which files it backs up, use a custom function for
;; `real-backup-filter-function'.  For example, to filter out
;; the saving of gnus .newsrc.eld files, do:
;;
;; (defun real-backup-no-newsrc-eld (filename)
;;   (cond
;;    ((string= (file-name-nondirectory filename) ".newsrc.eld") nil)
;;    (t t)))
;; (setq real-backup-filter-function 'real-backup-no-newsrc-eld)

;;; ChangeLog
;; v1.0 -> v1.1:  added real-backup-filter-function
;; v1.1 -> v1.2:  1) added real-backup-size-limit
;;                2) fixed "Local Variables" docs, which was inadvertently
;;                   being activated
;; v1.2 -> v1.3:  fix for some emacsen not having `file-remote-p'
;; v1.3 -> v1.4:  added footer and autoload
;; v1.4 -> v2.0:  refactor, deprecate old Emacs
;; v2.0 -> v2.1:  1) more features and tweaks
;;                2) add `real-backup-cleanup' and `real-backup-auto-cleanup'
;;                3) add `real-backup-open-backup'
;; v2.1 -> v3.0:  rebrand the package as `real-backup'
;; v3.0 -> v3.1:  add compression support

;;; Code:

(autoload 'cl-set-difference "cl-seq")

(defvar real-backup-directory (locate-user-emacs-file "real-backup/"))

(defvar real-backup-remote-files t
  "Whether to backup remote files at each save.

Defaults to nil.")

(defconst real-backup-time-format "%Y-%m-%d-%H-%M-%S"
  "Format given to `format-time-string' which is appended to the filename.")

(defconst real-backup-time-match-regexp "[[:digit:]]\\{4\\}\\(-[[:digit:]]\\{2\\}\\)\\{5\\}"
  "A regexp that matches `real-backup-time-format'.")

(defvar real-backup-filter-function #'identity
  "Function which should return non-nil if the file should be backed up.")

(defvar real-backup-size-limit (* 1 1024 1024)
  "Maximum size of a file (in bytes) that should be copied at each savepoint.

If a file is greater than this size, don't make a backup of it.
Setting this variable to nil disables backup suppressions based
on size.")

(defvar real-backup-cleanup-keep 20
  "Number of copies to keep for each file in `real-backup-cleanup'.")

(defvar real-backup-auto-cleanup nil
  "Automatically cleanup after making a backup.")

(defvar real-backup-compress t
  "Compress the backup files.")

(defvar real-backup-compression-program (or (executable-find "zstd") (executable-find "gzip"))
  "Compression program to be used when `real-backup-compress' is enabled.")

(defvar real-backup-compression-program-args (when (executable-find "zstd") "--rm"))

(defun real-backup ()
  "Perform a backup the `buffer-file-name' if needed."
  (let* ((filename (buffer-file-name))
         (backup-filename (real-backup-compute-location filename 'unique)))
    (when (and (or real-backup-remote-files (not (file-remote-p filename)))
               (funcall real-backup-filter-function filename)
               (or (not real-backup-size-limit) (<= (buffer-size) real-backup-size-limit)))
      (copy-file filename backup-filename t t t)
      (when real-backup-compress
        (let ((default-directory (file-name-directory backup-filename))
              (file (file-name-nondirectory backup-filename)))
          (start-process-shell-command
           "real-backup-compress" nil
           (concat real-backup-compression-program " " real-backup-compression-program-args " " file))))
      (when real-backup-auto-cleanup (real-backup-cleanup filename)))))

(defun real-backup-compute-location (filename &optional unique)
  "Compute backup location for FILENAME.

When UNIQUE is provided, add a unique timestamp after the file name."
  (let* ((localname (or (file-remote-p filename 'localname) filename))
         (method (or (file-remote-p filename 'method) "local"))
         (host (or (file-remote-p filename 'host) "localhost"))
         (user (or (file-remote-p filename 'user) user-real-login-name))
         (containing-dir (file-name-directory localname))
         (backup-dir (funcall #'file-name-concat real-backup-directory method host user containing-dir))
         (backup-basename (format "%s%s" (file-name-nondirectory localname) (if unique (concat "#" (format-time-string real-backup-time-format)) ""))))
    (when (not (file-exists-p backup-dir))
      (make-directory backup-dir t))
    (expand-file-name backup-basename backup-dir)))

(defun real-backup-backups-for-file (filename)
  "List of backups for FILENAME."
  (let* ((backup-filename (real-backup-compute-location filename))
         (backup-dir (file-name-directory backup-filename)))
    (directory-files backup-dir nil (concat "^" (regexp-quote (file-name-nondirectory backup-filename)) "#" real-backup-time-match-regexp "\\(\\.[[:alnum:]]+\\)?" "$"))))

;;;###autoload
(defun real-backup-cleanup (filename)
  "Cleanup backups of FILENAME, keeping `real-backup-cleanup-keep' copies."
  (interactive (list buffer-file-name))
  (if (not filename)
      (user-error "This buffer is not visiting a file")
    (let* ((backup-dir (file-name-directory (real-backup-compute-location filename)))
           (backup-files (real-backup-backups-for-file filename)))
      (dolist (file (cl-set-difference backup-files (last backup-files real-backup-cleanup-keep) :test #'string=))
        (let ((fname (expand-file-name file backup-dir)))
          (delete-file fname t))))))

;;;###autoload
(defun real-backup-open-backup (filename)
  "Open a backup of FILENAME or the current buffer."
  (interactive (list buffer-file-name))
  (if (not filename)
      (user-error "This buffer is not visiting a file")
    (let* ((current-major-mode major-mode)
           (default-dir default-directory)
           (backup-dir (file-name-directory (real-backup-compute-location filename)))
           (completion-extra-properties
            `(:annotation-function
              ,(lambda (bak) (format "   [%s bytes]" (file-attribute-size (file-attributes (expand-file-name bak backup-dir)))))))
           (backup-file (completing-read "Select file: " (real-backup-backups-for-file buffer-file-name))))
      (with-current-buffer (find-file (expand-file-name backup-file backup-dir))
        ;; Apply the same major mode and the same default directory as the original file
        (funcall current-major-mode)
        (setq-local default-directory default-dir)
        (read-only-mode 1)))))

;;;###autoload
(define-minor-mode real-backup-mode
  "Automatically backup files after saving them."
  :init-value nil
  :lighter " Backup"
  :global t
  (if real-backup-mode
      (add-hook 'after-save-hook 'real-backup)
    (remove-hook 'after-save-hook 'real-backup)))


(provide 'real-backup)
;;; real-backup.el ends here
