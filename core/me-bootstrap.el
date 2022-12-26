;; me-bootstrap.el --- Bootstrap packages (straight & use-package) -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(setq package-enable-at-startup nil)

(setq straight-base-dir minemacs-local-dir
      straight-repository-branch "develop"
      straight-vc-git-default-clone-depth '(1 single-branch)
      straight-build-dir (format "build-%s" emacs-version)
      straight-check-for-modifications nil)

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
