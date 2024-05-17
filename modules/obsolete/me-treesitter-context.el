;;; me-treesitter-context.el --- Treesit-based context and code folding -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package treesitter-context
  :straight (:host github :repo "zbelial/treesitter-context.el")
  :when (+emacs-features-p 'tree-sitter)
  :commands treesitter-context-mode
  :custom
  (treesitter-context-idle-time 0.5)
  :config
  (setq treesitter-context-background-color "#EEEEEE"
        treesitter-context-border-color "#000000"))

(use-package treesitter-context-fold
  :hook (prog-mode . treesitter-context-fold-mode)
  :when (+emacs-features-p 'tree-sitter)
  :init
  (+fn-inhibit-messages! treesitter-context-fold-mode)
  :config
  (require 'treesitter-context))


(provide 'obsolete/me-treesitter-context)

;;; me-treesitter-context.el ends here
