;;; me-org-modern-indent.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-14
;; Last modified: 2025-08-14

;;; Commentary:

;;; Code:


;; Add `org-indent' styling for `org-modern'
(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :hook (org-modern-mode . org-modern-indent-mode))


(provide 'obsolete/me-org-modern-indent)
;;; me-org-modern-indent.el ends here
