;;; me-clink.el --- Clink  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-08
;; Last modified: 2025-07-08

;;; Commentary:

;;; Code:


;; Clink integration to Emacs
(use-package clink
  :straight (:host github :repo "abougouffa/clink.el")
  :when (featurep 'feat/sqlite3)
  :hook (minemacs-first-c/c++-file . global-clink-mode))


(provide 'obsolete/me-clink)
;;; me-clink.el ends here
