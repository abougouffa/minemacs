;;; me-graphql.el --- GraphQL -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-graphql
  :auto-mode '((("\\.gql\\'" "\\.graphql\\'") . graphql-mode)))

(use-package graphql-mode
  :straight t)


(provide 'on-demand/me-graphql)
;;; me-graphql.el ends here
