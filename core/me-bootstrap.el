;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

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

;; Configure `use-package'
(unless (require 'use-package nil t)
  (straight-use-package 'use-package))

(cl-callf append straight-built-in-pseudo-packages
  '(treesit ; Some packages like `ts-movement' depends on it
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

;; Extra utilities
;; Be cautious about the installed revision of `once' and `satch' as they aren't stable yet
(use-package once
  :straight (:host github :repo "emacs-magus/once")
  :pin-ref "a6f950c29c846a50018bc63695f24f611c1a58be")

(use-package satch
  :straight (:host github :repo "emacs-magus/satch.el")
  :pin-ref "77993b711cccf16702fdc8d21d8f8ba10d7bd0fb")


(provide 'me-bootstrap)

;;; me-bootstrap.el ends here
