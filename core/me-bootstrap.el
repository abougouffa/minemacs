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

;; Add the `:pin-ref' extension to integrate `straight' with `use-package'. And
;; add support for `minemacs-disabled-packages'.
(require 'me-use-package-extra)

(setq
 ;; Set `use-package' to verbose when MinEmacs is started in verbose mode
 use-package-verbose (cond (minemacs-debug 'debug) (minemacs-verbose t))
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer t
 ;; Make the expanded code as minimal as possible, do not try to catch errors
 use-package-expand-minimally (not minemacs-debug))


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
