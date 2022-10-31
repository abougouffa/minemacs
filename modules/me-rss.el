;; -*- lexical-binding: t; -*-

(use-package elfeed
  :straight t
  :defer t
  :general
  (+map "of" #'elfeed)
  :custom
  (elfeed-db-directory (concat minemacs-local-dir "elfeed")))


(provide 'me-rss)
