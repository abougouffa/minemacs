;;; me-gtd.el --- Getting Things Done (GTD) workflow for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package org-gtd
  :straight t
  :custom
  (org-gtd-directory (+directory-ensure org-directory "gtd/")))


(provide 'me-gtd)

;;; me-gtd.el ends here
