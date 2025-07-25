;;; me-multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")
;; Created: 2022-09-20
;; Last modified: 2025-07-26

;;; Commentary:

;;; Code:


;; Modify multiple occurrences simultaneously
(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))


(provide 'me-multi-cursors)

;;; me-multi-cursors.el ends here
