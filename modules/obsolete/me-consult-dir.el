;;; me-consult-dir.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Insert paths into the minibuffer prompt
(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir) ; Instead of `list-directory
         :package vertico
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))


(provide 'obsolete/me-consult-dir)
;;; me-consult-dir.el ends here
