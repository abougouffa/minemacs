;;; me-elm.el --- Elm language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-elm
  :auto-mode '(("\\.elm\\'" . elm-mode))
  :companion-packages '((elm-mode . elm-test-runner)))


;; Major mode for Elm
(use-package elm-mode
  :straight t)


;; Enhanced support for running `elm-test'
(use-package elm-test-runner
  :straight t)


(provide 'on-demand/me-elm)
;;; me-elm.el ends here
