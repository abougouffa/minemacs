;;; me-rg.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-18
;; Last modified: 2026-11-18

;;; Commentary:

;;; Code:


;; Emacs search tool based on "ripgrep"
(use-package rg
  :ensure t
  :bind (("C-c s" . rg-menu))
  :init
  (+super-project-define-commands 'rg-project))


(provide 'obsolete/me-rg)
;;; me-rg.el ends here
