;;; me-nushell.el --- Nushell scripts -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-30
;; Last modified: 2026-08-07

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-nushell
  :auto-mode '(("\\.nu\\'" . nushell-ts-mode))
  :interpreter-mode '(("nu" . nushell-ts-mode)))


;; Major mode for Nushell scripts (deprecated, used as a fallback if case `treesit' is not available)
(use-package nushell-mode
  :straight t
  :unless (featurep 'feat/tree-sitter))


;; Major mode for Nushell scripts
(use-package nushell-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(nu "https://github.com/nushell/tree-sitter-nu"))
  (treesit-ensure-installed 'nu))


(provide 'on-demand/me-nushell)
;;; me-nushell.el ends here
