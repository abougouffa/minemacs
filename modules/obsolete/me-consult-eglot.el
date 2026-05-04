;;; me-consult-eglot.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-05-04
;; Last modified: 2026-05-04

;;; Commentary:

;;; Code:


;; Consult integration with Eglot
(use-package consult-eglot
  :straight t
  :unless (+package-disabled-p 'consult 'me-completion)
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t))))


(provide 'obsolete/me-consult-eglot)
;;; me-consult-eglot.el ends here
