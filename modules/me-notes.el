;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package org-roam
  :straight t
  :hook (org-roam-mode . org-roam-db-autosync-enable)
  :init
  (+map! :infix "n"
    "f" #'org-roam-node-find
    "r" #'org-roam-ref-find
    "i" #'org-roam-node-insert
    "R" #'org-roam-node-random
    "B" #'org-roam-buffer-display-dedicated))

(use-package org-roam-protocol
  :after org-roam
  :demand t
  :custom
  (org-roam-protocol-store-links t)
  ;; Add this as bookmarklet in your browser
  ;; javascript:location.href='org-protocol://roam-ref?template=r&ref=%27+encodeURIComponent(location.href)+%27&title=%27+encodeURIComponent(document.title)+%27&body=%27+encodeURIComponent(window.getSelection())
  (org-roam-capture-ref-templates
   '(("r" "ref" plain "%?"
      :if-new (file+head "web/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+created: %U\n\n${body}\n")
      :unnarrowed t)))
  :config
  ;; Save a local snapshot of the captured web page using "single-file-cli"
  (advice-add
   'org-roam-protocol-open-ref :after
   (defun +org-roam-protocol--single-file-snapshot-a (info)
     (+single-file
      (plist-get info :ref)
      (+file-name-incremental
       (expand-file-name
        (concat "web/snapshots/" (+clean-file-name (plist-get info :title)) ".html")
        org-roam-directory))))))

(use-package org-roam-ui
  :straight t
  :init
  (+map! "nu" #'org-roam-ui-open))

(use-package consult-org-roam
  :straight t
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
