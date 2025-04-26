;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(require 'me-lib)
(require 'use-package)

(setq
 ;; Base directory
 straight-base-dir minemacs-local-dir
 ;; Add Emacs version and the Git hash to the build directory to avoid problems
 straight-build-dir (format "build-%s%s" emacs-version (if emacs-repository-version (format "-%s" (substring emacs-repository-version 0 8)) ""))
 ;; Use the "develop" branch on straight.el's repo
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

;; HACK+PERF: Reduce installation time and disk usage using "--filter=tree:0".
;; This cuts the size of the "straight/repos" directory by more than half (from
;; 807M to 362M) while keeping it possible to checkout and download older
;; commits on-demand (unlike "--depth=N"). The parameter is injected when a "git
;; clone" command call is requested in `straight--process-run' which is called
;; from `straight-vc-git--clone-internal'
(advice-add
 'straight--process-run :filter-args
 (lambda (args)
   (if (and (equal (car args) "git") (equal (cadr args) "clone"))
       `("git" "clone" "--filter=tree:0" ,@(cddr args))
     args)))

(cl-callf append straight-built-in-pseudo-packages
  '(treesit ; Some packages like `ts-movement' depends on it
    docker-tramp)) ; Needed by some packages like `ros', but provided by `tramp'

(use-package compat
  :straight (compat :source gnu-elpa-mirror)
  :when (< emacs-major-version 30)
  :demand)


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
