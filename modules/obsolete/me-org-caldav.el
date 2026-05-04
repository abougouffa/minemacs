;;; me-org-caldav.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-05-04
;; Last modified: 2026-05-04

;;; Commentary:

;;; Code:


(use-package org-caldav
  :straight (:host github :repo "dengste/org-caldav")
  :custom
  (org-caldav-files (list (concat org-directory "appointements.org"))))


(provide 'obsolete/me-org-caldav)
;;; me-org-caldav.el ends here
