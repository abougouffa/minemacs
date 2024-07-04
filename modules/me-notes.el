;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Useful resources:
;; https://takeonrules.com/2022/10/01/exploring-the-denote-emacs-package/
;; https://takeonrules.com/2022/10/02/migration-plan-for-org-roam-notes-to-denote/
;; https://github.com/bitspook/notes-migrator

(use-package denote
  :straight t
  :commands denote-create-note denote-insert-link denote-show-backlinks-buffer
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-prompts '(title keywords)) ; These are the minimum viable prompts for notes
  (denote-file-type 'org) ; I love org-mode format; reading ahead I'm setting this
  (denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech
  :config
  (denote-rename-buffer-mode 1))


(provide 'me-notes)

;;; me-notes.el ends here
