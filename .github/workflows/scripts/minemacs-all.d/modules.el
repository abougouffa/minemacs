;; modules.el --- Enable all modules in CI mode -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(setq minemacs-core-modules '(me-splash me-keybindings me-evil me-core-ui me-completion)
      minemacs-modules (mapcar #'intern (mapcar #'file-name-sans-extension (directory-files minemacs-modules-dir nil "\\.el\\'"))))
