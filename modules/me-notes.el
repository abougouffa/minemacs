;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Useful resources:
;; https://takeonrules.com/2022/10/01/exploring-the-denote-emacs-package/
;; https://takeonrules.com/2022/10/02/migration-plan-for-org-roam-notes-to-denote/
;; https://github.com/bitspook/notes-migrator

(use-package denote
  :straight t
  :commands denote-create-note denote-insert-link denote-show-backlinks-buffer
  :init
  (+map! :infix "n"
    "n" #'denote-create-note
    "o" #'denote-open-or-create
    "j" #'denote-journal-extras-new-or-existing-entry
    "J" #'denote-journal-extras-new-entry
    "l" #'denote-insert-link
    "L" #'denote-add-links
    "m" #'denote-add-missing-links
    "b" #'denote-show-backlinks-buffer)
  :custom
  (denote-prompts '(title keywords)) ; These are the minimum viable prompts for notes
  (denote-file-type 'org) ; I love org-mode format; reading ahead I'm setting this
  (denote-date-prompt-use-org-read-date t)) ; And `org-read-date' is an amazing bit of tech

(use-package consult-notes
  :straight t
  :init
  (defun +consult-notes--unbound-org-roam ()
    (fmakunbound 'consult-notes-org-roam-mode)
    (fmakunbound 'consult-notes-org-roam-find-node-relation))
  (+consult-notes--unbound-org-roam)
  (+map! :infix "n"
    "f" #'consult-notes
    "s" #'consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources `(("Notes" "n" ,denote-directory))) ; Set notes dir(s), see below
  (consult-notes-denote-files-function #'denote-directory-text-only-files) ; Search only for text files in denote dir
  :config
  (+consult-notes--unbound-org-roam)
  (consult-notes-denote-mode 1)
  (consult-notes-org-headings-mode 1))


(provide 'me-notes)

;;; me-notes.el ends here
