;;; me-packages-managers.el --- Guix & Nix integration into Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package guix
  :straight t
  :when (executable-find "guix"))

(use-package nix-update
  :straight t)


(provide 'obsolete/me-packages-managers)
;;; me-packages-managers.el ends here
