;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(setq
 ;; Base directory
 straight-base-dir minemacs-local-dir
 ;; Add Emacs version and the Git hash to the build directory to avoid problems
 straight-build-dir (format "build-%s%s" emacs-version
                            (if emacs-repository-version
                                (format "-%s" (substring emacs-repository-version 0 8))
                              ""))
 ;; Use the "develop" branch on straight.el's repo.
 straight-repository-branch "develop"
 ;; Do not slow startup by checking for package modifs, check only on demand
 straight-check-for-modifications '(check-on-save find-when-checking))

;; Bootstraping straight.el
;; See: github.com/radian-software/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file (concat straight-base-dir "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure `use-package'
(unless (require 'use-package nil t)
  (straight-use-package 'use-package))

;; Add the `:pin-ref' extension to `use-package'
(require 'me-use-package-pin-ref)

(setq
 ;; Set `use-package' to verbose when MinEmacs is started in verbose mode
 use-package-verbose minemacs-verbose
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer t)

(defvar minemacs--build-functions nil
  "These functions are run after completing package updates.")

(defun +register-build-function (fn)
  "Register build function FN to be called at the end of `minemacs-update'."
  (add-to-list 'minemacs--build-functions fn))

(defun minemacs-update ()
  "Update MinEmacs packages."
  (interactive)
  ;; Backup the current installed versions, this file can be restored if version
  ;; upgrade does break some packages.
  (message "[MinEmacs]: Creating backups for the current versions of packages")
  (let* ((backup-dir (concat minemacs-local-dir (format "minemacs/versions/")))
         (dest-file (concat backup-dir "default-" (format-time-string "%Y%m%d%H%M%S") ".el"))
         (src-file (concat straight-base-dir "straight/versions/default.el")))
    (unless (file-directory-p backup-dir) (mkdir backup-dir :parents))
    (when (file-exists-p src-file)
      (message "[MinEmacs]: Creating backup from \"%s\" to \"%s\"" src-file dest-file)
      (copy-file src-file dest-file)))

  ;; Run `straight's update cycle, taking into account the explicitly pinned
  ;; packages versions.
  (message "[MinEmacs]: Pulling packages")
  (straight-x-pull-all)
  (message "[MinEmacs]: Freezing packages")
  (straight-x-freeze-versions)
  (message "[MinEmacs]: Rebuilding packages")
  (straight-rebuild-all)

  ;; Runn package-specific build functions (ex: `pdf-tools-install-noverify')
  (message "[MinEmacs]: Running additional package-specific build functions")
  (dolist (fn minemacs--build-functions)
    (message "MinEmacs: Running `%s'" fn)
    ;; Do not ask before installing
    (cl-letf (((symbol-function 'yes-or-no-p) #'always)
              ((symbol-function 'y-or-n-p) #'always))
      (funcall fn))))


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
