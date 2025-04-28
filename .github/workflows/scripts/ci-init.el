;; ci-init.el --- This file is used as an init script in the CI -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-10-22
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(setenv "MINEMACS_DEBUG" "1")
(setenv "MINEMACS_VERBOSE" "1")
(setenv "MINEMACS_NOT_LAZY" "1")

(message "Running MinEmacs in CI mode, loading all packages.")

(let* ((scripts-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root-dir (expand-file-name "../../../" scripts-dir)))
  (message "Calculated root directory is \"%s\"" root-dir)
  (message "Loading \"early-init.el\"")
  (load (expand-file-name "early-init.el" root-dir))

  (message "Loading \"init.el\"")
  (load (expand-file-name "init.el" root-dir)))
