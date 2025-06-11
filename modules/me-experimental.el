;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-06-11

;;; Commentary:

;;; Code:


;; Extra non-standard functionalities for Eglot
(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :commands (eglot-x-setup))


;; The Emacs Gerrit Experience
(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee"))


;; Gerrit integration from ChromiumOS development utils
(use-package gerrit
  :straight (chromiumos-dev-utils :type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/*")))


;; Transient menus to use some "repo" commands within Magit
(use-package repo-transient
  :straight chromiumos-dev-utils
  :commands (repo-main-menu))


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit
  :straight `( :host github :repo "akirakyle/emacs-webkit"
               :files (:defaults "*.js" "*.css" "*.so")
               :pre-build ,(if (featurep 'os/win)
                               '(message "The `webkit' module won't build on Windows.")
                             '("make")))
  :when (and (featurep 'feat/modules) (not (featurep 'os/win))))


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit-dark
  :straight webkit
  :when (and (featurep 'feat/modules) (not (featurep 'os/win)))
  :bind (:map webkit-mode-map ("C-c d" . webkit-dark-toggle)))


(provide 'me-experimental)
;;; me-experimental.el ends here
