;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Highlight inactive code regions with eglot power (mainly C/C++ preprocessor directives)
(use-package eglot-inactive-regions
  :straight t
  :commands (eglot-inactive-regions-mode))


;; Emacs search engine which combines concepts from information retrievial and Bayesian search theory
(use-package p-search
  :straight (:host github :repo "zkry/p-search")
  :commands (p-search))


;; Work seamlessly with GitHub gists from Emacs
(use-package igist
  :straight t)


(provide 'me-experimental)
;;; me-experimental.el ends here
