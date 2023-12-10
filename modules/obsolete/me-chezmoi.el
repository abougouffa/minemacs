;;; me-chezmoi.el --- Dotfiles management with chezmoi -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package chezmoi
  :straight (chezmoi :files (:defaults "extensions/*.el"))
  :when (executable-find "chezmoi")
  :commands
  chezmoi-find chezmoi-write chezmoi-diff chezmoi-open-other
  chezmoi-sync-files chezmoi-magit-status
  :init
  (+map! :infix "o"
    "c" '(nil :wk "chezmoi")
    "cf" #'chezmoi-find
    "cw" #'chezmoi-write
    "cd" #'chezmoi-diff
    "co" #'chezmoi-open-other
    "cs" #'chezmoi-sync-files))

(use-package chezmoi-ediff
  :when (executable-find "chezmoi")
  :commands chezmoi-ediff
  :init
  (+map! "oce" #'chezmoi-ediff))

(use-package chezmoi-magit
  :when (executable-find "chezmoi")
  :commands chezmoi-magit-status
  :init
  (+map! "ocg" #'chezmoi-magit-status))


(provide 'obsolete/me-chezmoi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-chezmoi.el ends here
