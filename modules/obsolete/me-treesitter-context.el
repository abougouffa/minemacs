;;; me-treesitter-context.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-05-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; A `treesit'-based package to show code context, dim surrouding text, and fold code
(use-package treesitter-context
  :vc (:url "https://github.com/zbelial/treesitter-context.el")
  :when (featurep 'feat/tree-sitter)
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
