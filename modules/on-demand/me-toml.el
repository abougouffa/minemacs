;;; me-toml.el --- TOML language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-toml
  :auto-mode '(("\\.toml\\'" . toml-mode))
  :companion-packages '((toml-ts-mode . toml-mode)))

(use-package toml-mode
  :straight t)


(provide 'on-demand/me-toml)
;;; me-toml.el ends here
