;;; me-julia.el --- Julia support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-julia
  :auto-mode '(("\\.jl\\'" . julia-mode))
  :companion-packages '(((julia-mode julia-ts-mode ess-julia-mode) . (julia-repl julia-snail julia-ts-mode))))


;; Major mode for editing Julia source code
(use-package julia-mode
  :straight t)


;; Major mode for Julia source code using Tree-sitter
(use-package julia-ts-mode
  :straight t
  :when (+emacs-options-p 'tree-sitter))


;; A minor mode for a Julia REPL
(use-package julia-repl
  :straight t)


;; An Emacs development environment for Julia
(use-package julia-snail
  :straight t)


(provide 'on-demand/me-julia)
;;; me-julia.el ends here
