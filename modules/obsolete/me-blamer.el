;;; me-blamer.el --- Blamer - show git blame information on each line -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package blamer
  :straight t
  :hook (minemacs-first-file . global-blamer-mode)
  :custom
  (blamer-min-offset 60)
  (blamer-prettify-time-p t)
  (blamer-author-formatter "%s ")
  (blamer-datetime-formatter "[%s], ")
  (blamer-commit-formatter "“%s”")
  (blamer-avatar-folder (concat minemacs-cache-dir "blamer-avatars/"))
  :config
  ;; Use 15% smaller font size for blamer's text
  (set-face-attribute 'blamer-face nil :height (max (truncate (* 0.9 (face-attribute 'default :height))) 1)))


(provide 'obsolete/me-blamer)

;;; me-blamer.el ends here
