;;; me-gambol.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-16
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Emacs text actions using LSP symbol information
(use-package gambol
  :vc (:url "https://codeberg.com/woolsweater/gambol.el")
  :hook (eglot-managed-mode . gambol-mode)
  :bind
  ( :map gambol-mode-map
    ("M-g ," . gambol:go-to-previous)
    ("M-g ." . gambol:go-to-next)
    ([remap mc/mark-all-dwim] . gambol:edit-all)
    :map gambol-repeat-map
    ("," . gambol:go-to-previous)
    ("." . gambol:go-to-next)
    ("e" . gambol:edit-all)
    ("o" . gambol:occur))
  :init
  (with-eval-after-load 'embark (gambol:install-embark-integration))) ; Integrate with `embark'


(provide 'obsolete/me-gambol)
;;; me-gambol.el ends here
