;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-05-05

;;; Commentary:

;;; Code:


;; Highlight inactive code regions with eglot power (mainly C/C++ preprocessor directives)
(use-package eglot-inactive-regions
  :straight t
  :commands (eglot-inactive-regions-mode))


;; Extra non-standard functionalities for Eglot
(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :commands (eglot-x-setup))


;; Work seamlessly with GitHub gists from Emacs
(use-package igist
  :straight t)


;; The Emacs Gerrit Experience
(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee"))


;; Gerrit integration
(use-package gerrit
  :straight t)


(provide 'me-experimental)
;;; me-experimental.el ends here
