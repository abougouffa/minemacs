;;; me-org-re-reveal.el --- Org Reveal integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package org-re-reveal
  :straight t)

(use-package oer-reveal
  :straight t
  :hook (minemacs-build-functions . oer-reveal-setup-submodules))

(use-package org-re-reveal-citeproc
  :straight t)


(provide 'obsolete/me-org-re-reveal)
;;; me-org-re-reveal.el ends here
