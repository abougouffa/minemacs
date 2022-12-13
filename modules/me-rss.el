;; -*- lexical-binding: t; -*-

(use-package elfeed
  :straight t
  :defer t
  :general
  (+map "of" #'elfeed)
  :custom
  (elfeed-db-directory (+expand 'local "elfeed/db" t)))


(provide 'me-rss)
