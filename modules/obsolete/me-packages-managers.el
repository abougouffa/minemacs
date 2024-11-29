;;; me-packages-managers.el --- Guix & Nix integration into Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package guix
  :ensure t
  :when (executable-find "guix"))

(use-package nix-update
  :ensure t)


(provide 'obsolete/me-packages-managers)
;;; me-packages-managers.el ends here
