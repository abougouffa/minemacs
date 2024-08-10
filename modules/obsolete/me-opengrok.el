;;; me-opengrok.el --- Opengrok integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package eopengrok
  :straight t
  :commands
  (eopengrok-mode
   eopengrok-find-reference eopengrok-find-text eopengrok-find-definition eopengrok-find-custom
   eopengrok-jump-to-source eopengrok-create-index eopengrok-create-index-with-enable-projects))


(provide 'obsolete/me-opengrok)
;;; me-opengrok.el ends here
