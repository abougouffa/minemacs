;;; me-julia.el --- Julia support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-julia
  :auto-mode '(("\\.jl\\'" . julia-mode))
  :interpreter-mode '(("julia" . julia-mode))
  :companion-packages '(((julia-mode julia-ts-mode ess-julia-mode) . (julia-repl julia-snail julia-ts-mode))))


;; Major mode for editing Julia source code
(use-package julia-mode
  :ensure t
  :interpreter "julia")


;; Major mode for Julia source code using Tree-sitter
(use-package julia-ts-mode
  :ensure t
  :when (featurep 'feat/tree-sitter)
  :interpreter "julia"
  :config
  (add-to-list 'treesit-language-source-alist '(julia "https://github.com/tree-sitter/tree-sitter-julia"))
  (treesit-ensure-installed 'julia))


;; A minor mode for a Julia REPL
(use-package julia-repl
  :ensure t)


;; An Emacs development environment for Julia
(use-package julia-snail
  :ensure t)


(provide 'on-demand/me-julia)
;;; me-julia.el ends here
