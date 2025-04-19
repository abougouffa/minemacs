;;; me-lacarte.el --- Lacarte -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Execute menu items as commands, with completion
(use-package lacarte
  :straight t
  :bind ([f10] . lacarte-execute-menu-command))


(provide 'obsolete/me-lacarte)
;;; me-lacarte.el ends here
