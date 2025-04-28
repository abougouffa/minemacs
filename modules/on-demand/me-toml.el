;;; me-toml.el --- TOML language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-toml
  :auto-mode '(("\\.toml\\'" . toml-mode))
  :companion-packages '((toml-ts-mode . toml-mode)))


;; Major mode for editing TOML files
(use-package toml-mode
  :straight t)


(provide 'on-demand/me-toml)
;;; me-toml.el ends here
