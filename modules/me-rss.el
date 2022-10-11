;; -*- lexical-binding: t; -*-

(use-package elfeed
  :straight t
  :defer t
  :general
  (me-map "of" #'elfeed)
  :custom
  (elfeed-db-directory (expand-file-name "elfeed" minemacs-local-dir)))


(provide 'me-rss)
