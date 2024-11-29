;;; me-calendar.el --- Calendar integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; ;; Schedule your day visually, using timeblocking technique inside Emacs
;; (use-package org-timeblock
;;   :ensure t)


(use-package org-caldav
  :vc (:url "https://github.com/dengste/org-caldav")
  :custom
  (org-caldav-files (list (concat org-directory "appointements.org"))))


(provide 'me-calendar)

;;; me-calendar.el ends here
