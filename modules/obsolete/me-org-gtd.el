;;; me-org-gtd.el --- Getting Things Done -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; GTD workflow with Org mode
(use-package org-gtd
  :straight t
  :custom
  (org-gtd-directory (+directory-ensure org-directory "gtd/")))


(provide 'obsolete/me-org-gtd)
;;; me-org-gtd.el ends here
