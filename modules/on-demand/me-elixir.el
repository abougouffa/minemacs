;;; me-elixir.el --- Elixir -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-elixir
  :auto-mode '((("\\.elixir\\'" "\\.exs?\\'" "/mix\\.lock") . elixir-mode))
  :companion-packages '((elixir-mode . ob-elixir)))


;; Major mode for editing Elixir files
(use-package elixir-mode
  :straight t)


;; Org Babel code evaluation for Elixir
(use-package ob-elixir
  :straight t)


(provide 'on-demand/me-elixir)
;;; me-elixir.el ends here
