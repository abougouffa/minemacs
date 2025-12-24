;;; me-elixir.el --- Elixir -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-elixir
  :auto-mode '((("\\.elixir\\'" "\\.exs?\\'" "/mix\\.lock") . elixir-mode))
  :companion-packages '((elixir-mode . ob-elixir)))


;; Major mode for editing Elixir files
(use-package elixir-mode
  :ensure t)


;; Org Babel code evaluation for Elixir
(use-package ob-elixir
  :ensure t)


(provide 'on-demand/me-elixir)
;;; me-elixir.el ends here
