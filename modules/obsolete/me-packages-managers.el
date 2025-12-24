;;; me-packages-managers.el --- Guix & Nix integration into Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-04-11
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

(use-package guix
  :ensure t
  :when (executable-find "guix"))

(use-package nix-update
  :ensure t)


(provide 'obsolete/me-packages-managers)
;;; me-packages-managers.el ends here
