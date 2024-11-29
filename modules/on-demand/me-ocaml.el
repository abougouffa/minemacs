;;; me-ocaml.el --- OCaml support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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


;; Universal toplevel for OCaml
(use-package utop
  :ensure t)


(provide 'on-demand/me-ocaml)
;;; me-ocaml.el ends here
