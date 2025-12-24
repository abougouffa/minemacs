;;; me-coffee.el --- Coffee language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-coffee
  :auto-mode '((("\\.coffee\\'" "\\.iced\\'" "Cakefile\\'" "\\.cson\\'") . coffee-mode))
  :interpreter-mode '(("coffee" . coffee-mode)))


;; Major mode for CoffeeScript code
(use-package coffee-mode
  :ensure t)


(provide 'on-demand/me-coffee)
;;; me-coffee.el ends here
