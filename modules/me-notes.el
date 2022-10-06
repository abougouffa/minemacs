;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package org-roam
  :straight t
  :after org
  :general
  (me-map
    "nf" '(org-capture :which-key "Org-Roam find node")
    "nr" '(org-capture :which-key "Org-Roam find node")
    "nR" '(org-capture :which-key "Org-Roam find node"))
  :commands (org-roam
             org-roam-ref-find
             org-roam-node-find
             org-roam-node-open
             org-roam-node-insert
             org-roam-node-random))


(use-package org-roam-ui
  :straight t
  :commands (org-roam-ui-open org-roam-ui-sync-mode)
  :after org-roam
  :general
  (me-map
    "nR" '(org-roam-ui-open :which-key "Org-Roam UI"))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;; From https://org-roam.discourse.group/t/configure-deft-title-stripping-to-hide-org-roam-template-headers/478/10
(use-package deft
  :straight t
  :after org-roam
  :general
  (me-map
    "nd" '(deft :which-key "Deft"))
  :init
  (setq deft-directory org-roam-directory
        deft-recursive t
        deft-use-filter-string-for-filename t
        deft-default-extension "org")
  :config
  (defun +deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
     If `deft-use-filename-as-title' is nil, the title is taken to
     be the first non-empty line of the FILE.  Else the base name of the FILE is
     used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
          (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
        (deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'+deft-parse-title)

  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
                "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n" ;; org-roam ID
                "\\|\\[\\[\\(.*\\]\\)" ;; any link
                "\\)")))


(provide 'me-notes)
