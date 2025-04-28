;;; me-project-x.el --- project-x -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :commands (project-x-window-state-save project-x-window-state-load)
  :custom
  (project-x-local-identifier project-vc-extra-root-markers)
  :init
  (project-x-mode 1))


(provide 'obsolete/me-project-x)
;;; me-project-x.el ends here
