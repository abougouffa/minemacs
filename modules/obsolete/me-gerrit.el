;;; me-gerrit.el --- Gerrit integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package gerrit
  :straight (chromeos-gerrit :type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/repo-transient.el")))

(use-package gee
  :straight (:host bitbucket :repo "olanilsson/gee"))


(provide 'obsolete/me-gerrit)
;;; me-gerrit.el ends here
