;;; me-rss.el --- News and RSS -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package elfeed
  :straight t
  :defer t
  :general
  (+map "of" #'elfeed)
  :custom
  (elfeed-db-directory (+expand 'local "elfeed/db" t))
  (elfeed-enclosure-default-dir (+expand 'local "elfeed/enclosure" t)))


(provide 'me-rss)
