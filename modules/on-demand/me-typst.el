;;; me-typst.el --- Typst typing system -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-13
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-typst
  :auto-mode '(("\\.typ\\'" . typst-ts-mode)))


;; Typst tree sitter major mode for Emacs
(use-package typst-ts-mode
  :ensure t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst"))
  (treesit-ensure-installed 'typst)
  (+eglot-register '(typst-mode typst-ts-mode) '("tinymist")))


(use-package websocket ; Needed by `typst-preview'
  :ensure t)


;; Typst live preview minor mode
(use-package typst-preview
  :vc (:url "https://github.com/havarddj/typst-preview.el"))


;; Typst back-end for Org export engine
(use-package ox-typst
  :vc (:url "https://github.com/jmpunkt/ox-typst"))


(provide 'on-demand/me-typst)
;;; me-typst.el ends here
