;;; me-fun.el --- Some funny stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-12-27
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:

;; Implementation of an xkcd reader for Emacs
(use-package xkcd
  :ensure t
  :config
  (+ignore-root xkcd-cache-dir))


;; Practice touch/speed typing in Emacs
(use-package speed-type
  :ensure t)


;; Play Wordle (a.k.a. Lingo) in Emacs
(use-package wordel
  :ensure t)


(provide 'obsolete/me-fun)

;;; me-fun.el ends here
