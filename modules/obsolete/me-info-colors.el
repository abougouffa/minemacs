;;; me-info-colors.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-11-02
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Extra colors for `Info-mode'
(use-package info-colors
  :ensure t
  :hook (Info-selection . info-colors-fontify-node))


(provide 'obsolete/me-info-colors)
;;; me-info-colors.el ends here
