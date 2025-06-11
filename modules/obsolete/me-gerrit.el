;;; me-gerrit.el --- Gerrit stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-06-11
;; Last modified: 2025-06-11

;;; Commentary:

;;; Code:


;; The Emacs Gerrit Experience
(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee"))


;; Gerrit integration from ChromiumOS development utils
(use-package gerrit
  :straight (chromiumos-dev-utils :type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/*")))


;; Transient menus to use some "repo" commands within Magit
(use-package repo-transient
  :straight chromiumos-dev-utils
  :commands (repo-main-menu))


(provide 'obsolete/me-gerrit)
;;; me-gerrit.el ends here
