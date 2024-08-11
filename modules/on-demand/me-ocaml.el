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

(use-package tuareg
  :straight t)

(use-package opam-switch-mode
  :straight t)

(use-package dune
  :straight (:host github :repo "ocaml/dune" :depth 1 :files ("editor-integration/emacs/*.el")))

(use-package utop
  :straight t)


(provide 'on-demand/me-ocaml)
;;; me-ocaml.el ends here
