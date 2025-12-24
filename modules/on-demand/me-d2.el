;;; me-d2.el --- D2 -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-d2
  :auto-mode '(("\\.d2\\'" . d2-mode)))


;; Major mode for working with D2 graphs
(use-package d2-mode
  :ensure t
  :mode "\\.d2\\'")


;; Org Babel code evaluation for the D2 graph lanugage
(use-package ob-d2
  :ensure t
  :after minemacs-first-org-file ob
  :demand
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((d2 . t)))))


(provide 'on-demand/me-d2)
;;; me-d2.el ends here
