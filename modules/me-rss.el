;;; me-rss.el --- News and RSS -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package elfeed
  :straight t
  :general
  (+map "of" #'elfeed)
  :custom
  (elfeed-db-directory (concat minemacs-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat minemacs-local-dir "elfeed/enclosure/"))
  :init
  ;; Hide the annoying index file form recent files
  (add-to-list 'recentf-exclude  (concat minemacs-local-dir "elfeed/db/")))


(provide 'me-rss)
