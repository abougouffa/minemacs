;;; me-treesitter-context.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; A `treesit'-based package to show code context, dim surrouding text, and fold code
(use-package treesitter-context
  :straight (:host github :repo "zbelial/treesitter-context.el")
  :when (+emacs-options-p 'tree-sitter)
  :custom
  (treesitter-context-idle-time 0.5)
  (treesitter-context-show-context-always nil)
  :config
  (defun +treesitter-context--set-colors-from-theme (&rest _args)
    (setq treesitter-context-background-color (face-background 'hl-line)
          treesitter-context-border-color (face-foreground 'line-number)))

  (+treesitter-context--set-colors-from-theme)
  (satch-add-hook '(enable-theme-functions disable-theme-functions) '+treesitter-context--set-colors-from-theme))


(provide 'obsolete/me-treesitter-context)
;;; me-treesitter-context.el ends here
