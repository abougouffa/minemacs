;;; me-fun.el --- Some funny stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package xkcd
  :straight t
  :general
  (+map "ox" #'xkcd)
  :custom
  (xkcd-cache-dir (+directory-ensure (concat minemacs-local-dir "xkcd/")))
  (xkcd-cache-latest (concat minemacs-local-dir "xkcd/latest"))
  :config
  (+map-key :keymaps 'xkcd-mode-map
    "j" #'xkcd-next
    "k" #'xkcd-prev
    "l" #'xkcd-get-latest
    "L" #'xkcd-get-latest-cached
    "<right>" #'xkcd-next
    "<left>" #'xkcd-prev
    "o" #'xkcd-open-browser
    "O" #'xkcd-open-explanation-browser
    "r" #'xkcd-rand
    "y" #'xkcd-copy-link))

(use-package speed-type
  :straight t
  :defer t)

(use-package asm-blox
  :straight t
  :defer t)


(provide 'me-fun)
