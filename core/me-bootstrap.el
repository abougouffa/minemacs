;;; bootstrap.el --- Bootstrap -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <hacko@laptop>
;; Keywords:

(setq package-enable-at-startup nil)

(setq straight-base-dir minemacs-var-dir
      straight-repository-branch "develop"
      straight-vc-git-default-clone-depth '(1 single-branch)
      straight-build-dir (format "build-%s" emacs-version)
      straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)


(provide 'me-bootstrap)
