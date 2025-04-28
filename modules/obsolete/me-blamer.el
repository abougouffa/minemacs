;;; me-blamer.el --- Blamer - show git blame information on each line -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-11-22
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package blamer
  :straight t
  :custom
  (blamer-min-offset 60)
  (blamer-prettify-time-p t)
  (blamer-author-formatter "%s ")
  (blamer-datetime-formatter "[%s], ")
  (blamer-commit-formatter "“%s”")
  (blamer-avatar-folder (concat minemacs-cache-dir "blamer-avatars/"))
  :config
  ;; Use 15% smaller font size for blamer's text
  (set-face-attribute 'blamer-face nil :height (max (truncate (* 0.85 (face-attribute 'default :height))) 1)))


(provide 'obsolete/me-blamer)
;;; me-blamer.el ends here
