;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(setq
 ;; Base directory
 straight-base-dir minemacs-local-dir
 ;; Add Emacs version to the build directory to avoid problems
 straight-build-dir (format "build-%s" emacs-version)
 ;; Use the "develop" branch on straight.el's repo
 straight-repository-branch "develop"
 ;; Do not clone all project history, just the last worktree (--depth 1)
 straight-vc-git-default-clone-depth '(1 single-branch)
 ;; I don't modify packages installed from straight, so don't wast me time
 straight-check-for-modifications nil)

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

(let ((use-package-recipe ; prefer built-in `use-package' in Emacs 29+
       (if (>= emacs-major-version 29) '(use-package :type built-in) 'use-package)))
  (straight-use-package use-package-recipe))

(setq use-package-verbose minemacs-verbose)

(provide 'me-bootstrap)
