;;; me-treesit-fold.el --- Folding with Treesitter -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-01
;; Last modified: 2025-12-24

;;; Commentary:

;; Replaced the built-in `hs-minor-mode' which using `treesit' grammar since
;; Emacs 31

;;; Code:


;; Tree-sitter based code folding
(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :when (featurep 'feat/tree-sitter))


(provide 'obsolete/me-treesit-fold)
;;; me-treesit-fold.el ends here
