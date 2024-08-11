;;; me-mermaid.el --- Mermaid -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-mermaid '(mermaid-mode ob-mermaid)
  :auto-mode '(("\\.mmd\\'" . mermaid-mode)))

(use-package mermaid-mode
  :straight t)

(use-package ob-mermaid
  :straight (:host github :repo "arnm/ob-mermaid")
  :after minemacs-first-org-file ob
  :demand
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((mermaid . t)))))


(provide 'on-demand/me-mermaid)
;;; me-mermaid.el ends here
