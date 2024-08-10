;;; me-graphql.el --- GraphQL -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-graphql 'graphql-mode
  :auto-mode '((("\\.gql\\'" "\\.graphql\\'") . graphql-mode)))

(use-package graphql-mode
  :straight t)


(provide 'modes/me-graphql)
;;; me-graphql.el ends here
