;;; me-dune.el --- Ocaml's Dune -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-06-02
;; Last modified: 2025-06-02

;;; Commentary:

;;; Code:


;; Integration with the dune build system
(use-package dune
  :straight (:host github :repo "ocaml/dune" :depth 1 :files ("editor-integration/emacs/*.el")))


(provide 'obsolete/me-dune)
;;; me-dune.el ends here
