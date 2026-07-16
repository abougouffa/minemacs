;;; me-graphql.el --- GraphQL -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-07-16

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-graphql
  :auto-mode '((("\\.gql\\'" "\\.graphqls?\\'") . graphql-mode)))


;; Major mode for editing GraphQL schemas
(use-package graphql-mode
  :straight t)


;; Tree-sitter based GraphQL mode
(use-package graphql-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter)
  :config
  (add-to-list 'treesit-language-source-alist '(graphql "https://github.com/bkegley/tree-sitter-graphql"))
  (treesit-ensure-installed 'graphql))


(provide 'on-demand/me-graphql)
;;; me-graphql.el ends here
