;;; me-mermaid.el --- Mermaid -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-11-20

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mermaid
  :auto-mode '(("\\.mmd\\'" . mermaid-mode)))


;; Major mode for working with Mermaid graphs
(use-package mermaid-mode
  :ensure t)


;; Org Babel support for Mermaid evaluation
(use-package ob-mermaid
  :ensure t
  :after minemacs-first-org-file ob
  :demand
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((mermaid . t)))))


(provide 'on-demand/me-mermaid)
;;; me-mermaid.el ends here
