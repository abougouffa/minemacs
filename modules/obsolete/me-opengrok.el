;;; me-opengrok.el --- Opengrok integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

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
