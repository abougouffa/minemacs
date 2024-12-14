;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'me-lib)
(require 'use-package)

(defvar straight-base-dir)
(defvar straight-build-dir)
(defvar straight-repository-branch)
(defvar straight-check-for-modifications)

(setq
 ;; Base directory
 straight-base-dir minemacs-local-dir
 ;; Add Emacs version and the Git hash to the build directory to avoid problems
 straight-build-dir (format "build-%s%s" emacs-version (if emacs-repository-version (format "-%s" (substring emacs-repository-version 0 8)) ""))
 ;; Use the "develop" branch on straight.el's repo.
 straight-repository-branch "develop"
 ;; Do not slow startup by checking for package modifs, check only on demand
 straight-check-for-modifications '(check-on-save find-when-checking))

;; Bootstrapping straight.el
;; See: https://github.com/radian-software/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file (concat straight-base-dir "straight/repos/straight.el/bootstrap.el"))
      (install-url "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el")
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'straight)

;; HACK+PERF: Reduce installation time and disk usage using "--filter=tree:0",
;; this cuts the size of the "repos" directory by more than half (from 807M to
;; 362M) while keeping it possible to download older commits on-demand (unlike
;; "--depth=N"). The parameter is injected in `straight--process-run' which is
;; called from `straight-vc-git--clone-internal'
(advice-add
 'straight--process-run :around
 (lambda (fn &rest a)
   (apply fn (if (equal (list (car a) (cadr a)) '("git" "clone")) `(,(car a) ,(cadr a) "--filter=tree:0" ,@(cddr a)) a))))

(cl-callf append straight-built-in-pseudo-packages
  '(org ; Otherwise, `straight' will try to install it as a dependency
    treesit ; Some packages like `ts-movement' depends on it
    docker-tramp)) ; Needed by some packages like `ros', but provided by `tramp'

(setq
 ;; Set `use-package' to verbose when MinEmacs is started in verbose mode
 use-package-verbose (cond (minemacs-debug-p 'debug) (minemacs-verbose-p t))
 ;; Defer loading packages by default, use `:demand' to force loading a package
 use-package-always-defer (not minemacs-always-demand-p)
 use-package-always-demand minemacs-always-demand-p
 ;; Make the expanded code as minimal as possible, do not try to catch errors
 use-package-expand-minimally (not minemacs-debug-p))

;; Add the `:pin-ref' extension to integrate `straight' with `use-package'. And
;; add support for `minemacs-disabled-packages'.
(require 'me-use-package-extra)

;; Update the builtin packages if needed
(dolist (pkg (seq-uniq (append minemacs-update-builtin-packages
                               (and (< emacs-major-version 30)
                                    '(which-key editorconfig)))))
  (straight-use-package `(,pkg :source gnu-elpa-mirror)))

(use-package compat
  :straight (compat :source gnu-elpa-mirror)
  :when (< emacs-major-version 30)
  :demand)


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
