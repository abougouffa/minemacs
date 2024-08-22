;;; me-elixir.el --- Elixir -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-elixir
  :auto-mode '((("\\.elixir\\'" "\\.exs?\\'" "/mix\\.lock") . elixir-mode))
  :companion-packages '((elixir-mode . ob-elixir)))

(use-package elixir-mode
  :straight t)

(use-package ob-elixir
  :straight t)


(provide 'on-demand/me-elixir)
;;; me-elixir.el ends here
