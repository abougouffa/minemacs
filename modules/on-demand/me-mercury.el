;;; me-mercury.el --- Mercury language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mercury
  :companion-packages '((mercury-mode . metal-mercury-mode)))


;; Concise mercury major mode
(use-package metal-mercury-mode
  :straight (:host github :repo "ahungry/metal-mercury-mode"))


(provide 'on-demand/me-mercury)
;;; me-mercury.el ends here
