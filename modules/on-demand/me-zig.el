;;; me-zig.el --- Zig language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-zig
  :auto-mode '(("\\.\\(zig\\|zon\\)\\'" . zig-mode)))


;; Major mode for the Zig programming language
(use-package zig-mode
  :straight t)


;; Tree-sitter based major mode for the Zig programming language
(use-package zig-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(zig "https://github.com/GrayJack/tree-sitter-zig"))
  (treesit-ensure-installed 'zig))


(provide 'on-demand/me-zig)
;;; me-zig.el ends here
