;;; me-nix.el --- Nix support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-04-30

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-nix
  :auto-mode '(("\\.nix\\'" . nix-mode)
               ("\\.nix\\'" . nix-ts-mode)))


;; Major mode for editing Nix files
(use-package nix-mode
  :straight t)


;; Tree-sitter based major mode for editing Nix files
(use-package nix-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter)
  :mode "\\.nix\\'"
  :config
  ;; Register Eglot servers on the `nix-ts-mode' in addition to the already configured `nix-mode'
  (with-eval-after-load 'eglot
    (when-let* ((server (assoc 'nix-mode eglot-server-programs)))
      (setcar server '(nix-mode nix-ts-mode)))))


(provide 'on-demand/me-nix)
;;; me-nix.el ends here
