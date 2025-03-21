;;; me-consult-notes.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package consult-notes
  :straight t
  :init
  (defun +consult-notes--unbound-org-roam ()
    (fmakunbound 'consult-notes-org-roam-mode)
    (fmakunbound 'consult-notes-org-roam-find-node-relation))
  (+consult-notes--unbound-org-roam)
  :custom
  (consult-notes-denote-files-function #'denote-directory-files) ; Search only for text files in denote dir
  (consult-notes-use-rg (and (executable-find "rg") t))
  :config
  (+consult-notes--unbound-org-roam)
  (consult-notes-denote-mode 1))


(provide 'obsolete/me-consult-notes)
;;; me-consult-notes.el ends here
