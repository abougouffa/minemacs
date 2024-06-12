;;; me-dashboard.el --- Dashboard for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package dashboard
  :straight t
  :after evil evil-collection
  :demand
  :unless (bound-and-true-p +dashboard-disable)
  :custom
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-center-content t)
  (dashboard-banner-ascii "MinEmacs")
  (dashboard-banner-logo-title "Welcome to MinEmacs!")
  (dashboard-items '((recents . 5) (projects . 5) (bookmarks . 5)))
  (dashboard-image-banner-max-width 600)
  (dashboard-projects-backend 'project-el)
  (dashboard-startup-banner (concat minemacs-assets-dir "images/minemacs.png"))
  :config
  ;; Avoid opening the dashboard when Emacs starts with an open file.
  (unless (cl-some #'buffer-file-name (buffer-list))
    (dashboard-open)))


(provide 'obsolete/me-dashboard)

;;; me-dashboard.el ends here
