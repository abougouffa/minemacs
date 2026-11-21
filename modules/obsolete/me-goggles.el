;;; me-goggles.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-06-01
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:


;; Pulse modified regions
(use-package goggles
  :ensure t
  :hook ((text-mode prog-mode) . goggles-mode))


(provide 'obsolete/me-goggles)
;;; me-goggles.el ends here
