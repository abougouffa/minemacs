;;; me-mercury.el --- Mercury language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mercury
  :companion-packages '((mercury-mode . metal-mercury-mode)))


;; Concise mercury major mode
(use-package metal-mercury-mode
  :vc (:url "https://github.com/ahungry/metal-mercury-mode"))


(provide 'on-demand/me-mercury)
;;; me-mercury.el ends here
