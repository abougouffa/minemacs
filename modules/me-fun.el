;;; me-fun.el --- Some funny stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package xkcd
  :straight t
  :init
  (+map! "ox" #'xkcd)
  :custom
  (xkcd-cache-dir (+directory-ensure minemacs-local-dir "xkcd/"))
  (xkcd-cache-latest (concat minemacs-local-dir "xkcd/latest"))
  :config
  (+ignore-root xkcd-cache-dir))

(use-package speed-type
  :straight t)

(use-package asm-blox
  :straight t)

(use-package wordel
  :straight t)


(provide 'me-fun)

;;; me-fun.el ends here
