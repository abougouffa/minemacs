;;; me-treemacs.el --- Treemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package treemacs
  :straight t
  :custom
  (treemacs-persist-file (concat minemacs-local-dir "treemacs/persist.el"))
  (treemacs-last-error-persist-file (concat minemacs-local-dir "treemacs/last-error-persist.el"))
  (treemacs-width 30)
  :config
  ;; Use the same height for the root node (project directory)
  (set-face-attribute 'treemacs-root-face nil :height 1.0))

(use-package treemacs-nerd-icons
  :straight t
  :after treemacs nerd-icons
  :demand t
  :config
  (treemacs-load-theme "nerd-icons"))


(provide 'obsolete/me-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-treemacs.el ends here
