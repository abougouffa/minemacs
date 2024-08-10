;;; me-nix.el --- Nix support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-nix '(nix-mode nix-ts-mode)
  :auto-mode '(("\\.nix\\'" . nix-mode)
               ("\\.nix\\'" . nix-ts-mode)))

(use-package nix-mode
  :straight t)

(use-package nix-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter)
  :mode "\\.nix\\'"
  :config
  ;; Register Eglot servers on the `nix-ts-mode' in addition to the already configured `nix-mode'
  (with-eval-after-load 'eglot
    (when-let ((server (assoc 'nix-mode eglot-server-programs)))
      (setcar server '(nix-mode nix-ts-mode)))))


(provide 'modes/me-nix)
;;; me-nix.el ends here
