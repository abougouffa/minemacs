;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Useful resources:
;; https://takeonrules.com/2022/10/01/exploring-the-denote-emacs-package/
;; https://www.thesing-online.de/blog/20230302T215700--denote-as-a-zettelkasten.html

;; Simple notes for Emacs with an efficient file-naming scheme
(use-package denote
  :straight t
  :commands (denote-create-note denote-insert-link denote-show-backlinks-buffer)
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-date-prompt-use-org-read-date t) ; And `org-read-date' is an amazing bit of tech
  :config
  (denote-rename-buffer-mode 1))


;; Use Consult in tandem with Denote
(use-package consult-denote
  :straight t
  :after consult
  :init
  (consult-denote-mode 1)
  :config
  ;; Prefer `ripgrep' and `fd' variants when available
  (when (executable-find "fd")
    (setopt consult-denote-find-command #'consult-fd))
  (when (executable-find "rg")
    (setopt consult-denote-grep-command #'consult-ripgrep)))


;; View and filter Denote files in a tabulated list
(use-package denote-menu
  :straight t)


(provide 'me-notes)

;;; me-notes.el ends here
