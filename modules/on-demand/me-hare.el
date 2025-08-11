;;; me-hare.el --- Hare language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-11
;; Last modified: 2025-08-11

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-hare
  :auto-mode '(("\\.ha\\'" . hare-ts-mode)))


;; Hare programming lanugage mode
(use-package hare-mode
  :straight (:host sourcehut :repo "laumann/hare-mode"))


;; Tree-sitter based mode for Hare
(use-package hare-ts-mode
  :straight (:host sourcehut :repo "p00f/hare-ts-mode")
  :config
  (add-to-list
   'treesit-language-source-alist
   '(hare "https://github.com/tree-sitter-grammars/tree-sitter-hare"))
  (treesit-ensure-installed 'hare))


(provide 'on-demand/me-hare)
;;; me-hare.el ends here
