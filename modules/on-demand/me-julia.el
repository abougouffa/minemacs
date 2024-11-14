;;; me-julia.el --- Julia support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-julia
  :auto-mode '(("\\.jl\\'" . julia-mode))
  :companion-packages '(((julia-mode julia-ts-mode ess-julia-mode) . (julia-repl julia-snail julia-ts-mode))))

(use-package julia-mode
  :straight t)

(use-package julia-ts-mode
  :straight t
  :when (+emacs-options-p 'tree-sitter))

(use-package julia-repl
  :straight t)

(use-package julia-snail
  :straight t)


(provide 'on-demand/me-julia)
;;; me-julia.el ends here
