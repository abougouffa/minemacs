;; ci-init.el --- This file is used as an init script in the CI -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(setenv "MINEMACS_DEBUG" "1")
(setenv "MINEMACS_VERBOSE" "1")
(setenv "MINEMACS_NOT_LAZY" "1")

(message "Running MinEmacs in CI mode, loading all packages.")

(let ((root-dir (file-name-directory ;; ./../../../ -> minemacs-root-dir
                 (directory-file-name
                  (file-name-directory
                   (directory-file-name
                    (file-name-directory
                     (directory-file-name
                      (file-name-directory load-file-name)))))))))
  (message "Calculated root directory is \"%s\"" default-directory)
  (message "Loading \"early-init.el\"")
  (load (expand-file-name "early-init.el" root-dir))

  (message "Loading \"init.el\"")
  (load (expand-file-name "init.el" root-dir)))

(message "Running MinEmacs in CI mode, loading all packages.")
(mapc #'require minemacs-configured-packages)
