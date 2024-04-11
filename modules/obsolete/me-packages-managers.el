;;; me-packages-managers.el --- Guix & Nix integration into Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package guix
  :straight t
  :when (executable-find "guix")
  :init
  (+map! "og" #'guix))

(use-package nix-update
  :straight t)


(provide 'obsolete/me-packages-managers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-packages-managers.el ends here
