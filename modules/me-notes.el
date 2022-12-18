;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package org-roam
  :straight t
  :after org minemacs-loaded
  :general
  (+map
    "nf" '(org-roam-node-find :wk "Org-Roam find node")
    "nr" '(org-roam-ref-find :wk "Org-Roam find ref")
    "ni" '(org-roam-ref-find :wk "Org-Roam insert node")
    "nR" '(org-roam-node-random :wk "Org-Roam random node")))

(use-package org-roam-ui
  :straight t
  :general
  (+map
    "nR" '(org-roam-ui-open :wk "Org-Roam UI")))

;; From https://org-roam.discourse.group/t/configure-deft-title-stripping-to-hide-org-roam-template-headers/478/10
(use-package deft
  :straight t
  :after org-roam
  :general
  (+map
    "nd" '(deft :wk "Deft"))
  :init
  (setq deft-default-extension "org")
  :custom
  (deft-directory org-roam-directory)
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-strip-summary-regexp
   (concat "\\("
           "[\n\t]" ;; blank
           "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
           "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n" ;; org-roam ID
           "\\|\\[\\[\\(.*\\]\\)" ;; any link
           "\\)"))
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

  (advice-add 'deft-parse-title :override #'+deft-parse-title))


(provide 'me-notes)
