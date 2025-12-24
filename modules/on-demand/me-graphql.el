;;; me-graphql.el --- GraphQL -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-graphql
  :auto-mode '((("\\.gql\\'" "\\.graphql\\'") . graphql-mode)))


;; Major mode for editing GraphQL schemas
(use-package graphql-mode
  :ensure t)


(provide 'on-demand/me-graphql)
;;; me-graphql.el ends here
