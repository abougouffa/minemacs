;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-11-07
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; Emacs package with utilities for embedded development with OpenOCD
(use-package embed
  :vc (:url "https://github.com/xal-0/embed-el"))


;; A set of Emacs modes for various Yocto/Bitbake file formats
(use-package bitbake
  :vc (bitbake-modes :url "https://bitbucket.org/olanilsson/bitbake-modes")
  :hook (bitbake-mode . bitbake-electric-mode)
  :config
  (require 'bitbake-insert)
  (require 'bitbake-electric))


(provide 'me-embedded)

;;; me-embedded.el ends here
