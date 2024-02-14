;;; backup-each-save.el --- backup each savepoint of a file

;; Copyright (C) 2004  Benjamin Rutt
;; Copyright (C) 2024  Abdelhak BOUGOUFFA

;; Author: Benjamin Rutt <brutt@bloomington.in.us>
;;         Abdelhak BOUGOUFFA
;; Version: 2.0

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
;; v1.1 -> v1.2:  i)  added backup-each-save-size-limit
;;                ii) fixed "Local Variables" docs, which was inadvertently
;;                    being activated
;; v1.2 -> v1.3:  fix for some emacsen not having `file-remote-p'
;; v1.3 -> v1.4: added footer and autoload
;; v1.4 -> v2.0: refactor, deprecate old Emacs

;;; Code:

(defvar backup-each-save-directory (locate-user-emacs-file "backup-each-save/"))

(defvar backup-each-save-remote-files t
  "Whether to backup remote files at each save.

Defaults to nil.")

(defvar backup-each-save-time-format "%Y-%m-%d-%H-%M-%S"
  "Format given to `format-time-string' which is appended to the filename.")

(defvar backup-each-save-filter-function #'identity
  "Function which should return non-nil if the file should be backed up.")

(defvar backup-each-save-size-limit 800000
  "Maximum size of a file (in bytes) that should be copied at each savepoint.

If a file is greater than this size, don't make a backup of it.
Setting this variable to nil disables backup suppressions based
on size.")

;;;###autoload
(defun backup-each-save ()
  "Perform a backup the `buffer-file-name' if needed."
  (let ((buff-file-name (buffer-file-name)))
    (when (and (or backup-each-save-remote-files (not (file-remote-p buff-file-name)))
               (funcall backup-each-save-filter-function buff-file-name)
               (or (not backup-each-save-size-limit) (<= (buffer-size) backup-each-save-size-limit)))
      (copy-file buff-file-name (backup-each-save-compute-location buff-file-name) t t t t))))

(defun backup-each-save-compute-location (filename)
  "Compute backup location for FILENAME."
  (let* ((localname (or (file-remote-p filename 'localname) filename))
         (method (or (file-remote-p filename 'method) "local"))
         (host (or (file-remote-p filename 'host) "localhost"))
         (user (or (file-remote-p filename 'user) user-real-login-name))
         (containing-dir (file-name-directory localname))
         (basename (file-name-nondirectory localname))
         (backup-dir (funcall #'file-name-concat backup-each-save-directory method host user containing-dir)))
    (when (not (file-exists-p backup-dir))
      (make-directory backup-dir t))
    (expand-file-name (format "%s#%s" basename (format-time-string backup-each-save-time-format)) backup-dir)))

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
