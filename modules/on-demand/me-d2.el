;;; me-d2.el --- D2 -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-d2 '(d2-mode ob-d2)
  :auto-mode '(("\\.d2\\'" . d2-mode)))

(use-package d2-mode
  :straight t
  :mode "\\.d2\\'")

(use-package ob-d2
  :straight t
  :after minemacs-first-org-file ob
  :demand
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((d2 . t)))))


(provide 'on-demand/me-d2)
;;; me-d2.el ends here
