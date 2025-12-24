;;; me-calendar.el --- Calendar integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-06-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;; Schedule your day visually, using timeblocking technique inside Emacs
(use-package org-timeblock
  :ensure t)


(use-package org-caldav
  :vc (:url "https://github.com/dengste/org-caldav")
  :custom
  (org-caldav-files (list (concat org-directory "appointements.org"))))


(provide 'me-calendar)

;;; me-calendar.el ends here
