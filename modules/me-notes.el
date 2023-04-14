;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package org-roam
  :straight t
  :init
  (+map! :infix "n"
    "f" #'org-roam-node-find
    "r" #'org-roam-ref-find
    "i" #'org-roam-node-insert
    "R" #'org-roam-node-random))

(use-package org-roam-ui
  :straight t
  :init
  (+map! "nR" #'org-roam-ui-open))

(use-package consult-org-roam
  :straight t
  :after org-roam
  :demand t
  :init
  (+map! :infix "n"
    "s" #'consult-org-roam-search
    "l" #'consult-org-roam-forward-links
    "b" #'consult-org-roam-backlinks
    "F" #'consult-org-roam-file-find)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r) ; custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode 1)
  ;; Eventually suppress previewing for certain functions
  (consult-customize consult-org-roam-forward-links :preview-key (kbd "M-.")))


(provide 'me-notes)

;;; me-notes.el ends here
