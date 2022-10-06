;; -*- lexical-binding: t; -*-

(use-package elfeed
  :straight t
  :defer t
  :general
  (me-map "or" #'elfeed)
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" minemacs-var-dir)))


(provide 'me-rss)
