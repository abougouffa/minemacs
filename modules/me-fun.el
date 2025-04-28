;;; me-fun.el --- Some funny stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;; Implementation of an xkcd reader for Emacs
(use-package xkcd
  :straight t
  :config
  (+ignore-root xkcd-cache-dir))


;; Practice touch/speed typing in Emacs
(use-package speed-type
  :straight t)


;; Play Wordle (a.k.a. Lingo) in Emacs
(use-package wordel
  :straight t)


(provide 'me-fun)

;;; me-fun.el ends here
