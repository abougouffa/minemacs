;;; me-toml.el --- TOML language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-toml
  :auto-mode '(("\\.toml\\'" . toml-mode))
  :companion-packages '((toml-ts-mode . toml-mode)))


;; Major mode for editing TOML files
(use-package toml-mode
  :ensure t)


(provide 'on-demand/me-toml)
;;; me-toml.el ends here
