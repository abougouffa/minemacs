;;; me-org-jira.el --- Jira in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-17
;; Last modified: 2025-05-17

;;; Commentary:

;;; Code:


;; Bring Jira and Org mode together
(use-package org-jira
  :straight (:host github :repo "ahungry/org-jira")
  :custom
  (org-jira-working-dir (+directory-ensure org-directory "jira/")))


(provide 'obsolete/me-org-jira)
;;; me-org-jira.el ends here
