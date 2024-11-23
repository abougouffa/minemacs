;;; me-treesit-fold.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Tree-sitter based code folding
(use-package treesit-fold
  :straight (:host github :repo "emacs-tree-sitter/treesit-fold")
  :when (+emacs-options-p 'tree-sitter))


(provide 'obsolete/me-treesit-fold)
;;; me-treesit-fold.el ends here
