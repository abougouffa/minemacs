;;; me-jira.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")
;; Created: 2024-12-06
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-jira
  :auto-mode '(("\\.confluence$" . jira-markup-mode)
               ("jira.*\\.txt$" . jira-markup-mode)))


(use-package jira-markup-mode
  :straight t)


(provide 'on-demand/me-jira)
;;; me-jira.el ends here
