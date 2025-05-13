;;; me-typst.el --- Typst typing system -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-05-13
;; Last modified: 2025-05-13

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-typst
  :auto-mode '(("\\.typ\\'" . typst-ts-mode)))


;; Typst tree sitter major mode for Emacs
(use-package typst-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter))


;; Typst live preview minor mode
(use-package typst-preview
  :straight (:host github :repo "havarddj/typst-preview.el"))


(provide 'on-demand/me-typst)
;;; me-typst.el ends here
