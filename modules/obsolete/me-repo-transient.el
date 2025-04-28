;;; me-repo-transient.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-21
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:


;; Transient menus to use some "repo" commands within Magit
(use-package repo-transient
  :straight (:type git :repo "https://chromium.googlesource.com/chromiumos/platform/dev-util" :files ("contrib/emacs/gerrit/repo-transient.el"))
  :commands (repo-main-menu))


(provide 'obsolete/me-repo-transient)
;;; me-repo-transient.el ends here
