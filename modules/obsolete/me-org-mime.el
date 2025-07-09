;;; me-org-mime.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@tznvy.pbz")
;; Created: 2025-07-09
;; Last modified: 2025-07-09

;;; Commentary:

;;; Code:


;; Send HTML email using Org-mode HTML export (alternative to `org-msg')
(use-package org-mime
  :straight t
  :when +mu4e-available-p
  :after mu4e org
  :demand
  :config
  ;; Do not export table of contents nor author name
  (setq org-mime-export-options '(:with-latex dvipng :section-numbers t :with-author nil :with-toc nil)))


(provide 'obsolete/me-org-mime)
;;; me-org-mime.el ends here
