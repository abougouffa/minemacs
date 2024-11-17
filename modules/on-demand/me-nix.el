;;; me-nix.el --- Nix support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
  :when (+emacs-options-p 'tree-sitter)
  :mode "\\.nix\\'"
  :config
  ;; Register Eglot servers on the `nix-ts-mode' in addition to the already configured `nix-mode'
  (with-eval-after-load 'eglot
    (when-let* ((server (assoc 'nix-mode eglot-server-programs)))
      (setcar server '(nix-mode nix-ts-mode)))))


(provide 'on-demand/me-nix)
;;; me-nix.el ends here
