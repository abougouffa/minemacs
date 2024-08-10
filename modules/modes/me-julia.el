;;; me-julia.el --- Julia support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-julia '(julia-mode julia-ts-mode julia-repl)
  :auto-mode '(("\\.jl\\'" . julia-mode))
  :companion-packages '((ess-julia-mode . julia-mode)))

(use-package julia-mode
  :straight t)

(use-package julia-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))


(provide 'modes/me-julia)
;;; me-julia.el ends here
