;;; me-typst.el --- Typst typing system -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-13
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-typst
  :auto-mode '(("\\.typ\\'" . typst-ts-mode)))


;; Typst tree sitter major mode for Emacs
(use-package typst-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst"))
  (treesit-ensure-installed 'typst))


;; Typst live preview minor mode
(use-package typst-preview
  :straight (:host github :repo "havarddj/typst-preview.el"))


(provide 'on-demand/me-typst)
;;; me-typst.el ends here
