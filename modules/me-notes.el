;;; me-notes.el --- Notes management -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package org-roam
  :straight t
  :general
  (+map :infix "n"
    "f" #'org-roam-node-find
    "r" #'org-roam-ref-find
    "i" #'org-roam-node-insert
    "R" #'org-roam-node-random))

(use-package org-roam-ui
  :straight t
  :general
  (+map
    "nR" '(org-roam-ui-open :wk "Org-Roam UI")))

(use-package consult-org-roam
  :straight t
  :hook (minemacs-lazy . consult-org-roam-mode)
  :general
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
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key (kbd "M-.")))

;; From https://org-roam.discourse.group/t/configure-deft-title-stripping-to-hide-org-roam-template-headers/478/10
(use-package deft
  :straight t
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
   (rx (group
        (or (any ?\n ?\t) ;; blanks
            (seq bol "#+" (one-or-more (any alpha ?_)) ":" (zero-or-more not-newline) eol) ;; org-mode metadata
            (seq bol ":PROPERTIES:" ?\n (one-or-more (group (one-or-more not-newline) ?\n)) ":END:" ?\n) ;; org-roam ID
            (seq bol ":properties:" ?\n (one-or-more (group (one-or-more not-newline) ?\n)) ":end:" ?\n) ;; org-roam ID
            (seq "[[" (group (zero-or-more not-newline) "]")))))) ;; any link
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
