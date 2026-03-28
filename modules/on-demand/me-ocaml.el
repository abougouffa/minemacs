;;; me-ocaml.el --- OCaml support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2026-03-29

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-ocaml
  :auto-mode `(("\\.mly\\'" . neocaml-menhir-mode)
               ("\\.mll\\'" . neocaml-ocamllex-mode)
               ("\\.ml\\'" . neocaml-mode)
               ("\\.mli\\'" . neocaml-interface-mode)
               ("[./]opam\\'" . neocaml-opam-mode)
               ("/dune\\(-project\\|-workspace\\)\\'" . neocaml-dune-mode)
               (,(rx "." (or "cmi" "cmo" "cmx" "cma" "cmxa" "cmxs" "cmt" "cmti") eol) . neocaml-objinfo-mode))
  :interpreter-mode '(("ocamlrun" . neocaml-mode) ("ocaml" . neocaml-mode)))


;;  A modern, TreeSitter-powered, Emacs major mode for OCaml
(use-package neocaml
  :straight (:host github :repo "bbatsov/neocaml")
  :config
  ;; Register neocaml modes with Eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((neocaml-mode neocaml-interface-mode) . ("ocamllsp")))))


;; An overlay on Eglot for editing OCaml code using LSP
(use-package ocaml-eglot
  :straight (:host github :repo "tarides/ocaml-eglot")
  :hook
  (neocaml-base-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure))


(provide 'on-demand/me-ocaml)
;;; me-ocaml.el ends here
