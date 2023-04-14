;;; me-rss.el --- News and RSS -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package elfeed
  :straight t
  :init
  (+map! "of" #'elfeed)
  :custom
  (elfeed-db-directory (concat minemacs-local-dir "elfeed/db/"))
  (elfeed-enclosure-default-dir (concat minemacs-local-dir "elfeed/enclosure/"))
  :config
  ;; Hide the annoying index file form recent files
  (+ignore-root elfeed-db-directory elfeed-enclosure-default-dir))


(provide 'me-rss)

;;; me-rss.el ends here
