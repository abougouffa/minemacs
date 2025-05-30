;;; me-ibuffer-project.el --- Ibuffer + project.el integration (replaced with projection-ibuffer) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-02-23
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . +ibuffer-project-h)
  :config
  ;; From Crafted Emacs
  (defun +ibuffer-project-h ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))


(provide 'obsolete/me-ibuffer-project)
;;; me-ibuffer-project.el ends here
