;;; me-ocaml.el --- OCaml support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-ocaml
  :auto-mode '(("\\.mly\\'" . tuareg-menhir-mode)
               (("\\.eliomi?\\'" "\\.ml[ip]?\\'") . tuareg-mode)
               ("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\|\\-workspace\\)?\\'" . dune-mode))
  :interpreter-mode '(("ocamlrun" . tuareg-mode) ("ocaml" . tuareg-mode)))


;; Major mode and REPL for the OCaml programming language
(use-package tuareg
  :ensure t)


;; Select OCaml opam switches via a menu
(use-package opam-switch-mode
  :ensure t)


(defconst +dune-path (concat minemacs-on-demand-modules-dir "third-party/dune/"))

;; Integration with the dune build system
(use-package dune
  :load-path +dune-path
  :commands (dune-mode dune-promote dune-runtest-and-promote)
  :mode ("\\(?:\\`\\|/\\)dune\\(?:\\.inc\\|\\-project\\|\\-workspace\\)?\\'" . dune-mode))


;; Integration with dune --watch tasks
(use-package dune-watch
  :after dune
  :demand)


;; Integrate `dune' with `flymake'
(use-package dune-flymake
  :after dune
  :demand)


;; Universal toplevel for OCaml
(use-package utop
  :ensure t)


(provide 'on-demand/me-ocaml)
;;; me-ocaml.el ends here
