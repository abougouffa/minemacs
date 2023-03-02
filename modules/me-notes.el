;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package org-roam
  :straight t
  :init
  (+map :infix "n"
    "f" #'org-roam-node-find
    "r" #'org-roam-ref-find
    "i" #'org-roam-node-insert
    "R" #'org-roam-node-random))

(use-package org-roam-ui
  :straight t
  :init
  (+map
    "nR" '(org-roam-ui-open :wk "Org-Roam UI")))

(use-package consult-org-roam
  :straight t
  :after org-roam
  :demand t
  :init
  (+map
    "ns" #'consult-org-roam-search
    "nl" #'consult-org-roam-forward-links
    "nb" #'consult-org-roam-backlinks
    "nF" #'consult-org-roam-file-find)
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  (consult-org-roam-buffer-narrow-key ?r) ; custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-after-buffers t)
  :config
  (consult-org-roam-mode 1)
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-.")))


(provide 'me-notes)
