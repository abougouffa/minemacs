;;; me-cypher.el --- Cypher (Neo4j) language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cypher
  :auto-mode '(("\\.cyp\\(?:her\\)?\\'" . cypher-mode)))


;; Major mode for editing Cypher scripts
(use-package cypher-mode
  :straight t)


(provide 'on-demand/me-cypher)
;;; me-cypher.el ends here
