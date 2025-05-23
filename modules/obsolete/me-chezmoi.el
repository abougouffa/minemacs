;;; me-chezmoi.el --- Dotfiles management with chezmoi -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-11-12
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package chezmoi
  :straight (:files (:defaults "extensions/*.el"))
  :when (executable-find "chezmoi")
  :commands (chezmoi-find chezmoi-write chezmoi-diff chezmoi-open-other chezmoi-sync-files chezmoi-magit-status))

(use-package chezmoi-ediff
  :when (executable-find "chezmoi")
  :commands (chezmoi-ediff))

(use-package chezmoi-magit
  :when (executable-find "chezmoi")
  :commands (chezmoi-magit-status))


(provide 'obsolete/me-chezmoi)
;;; me-chezmoi.el ends here
