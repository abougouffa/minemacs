;;; me-spacious-padding.el --- Spacious padding -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package spacious-padding
  :straight t
  :hook (minemacs-after-startup . spacious-padding-mode)
  :custom
  (spacious-padding-subtle-mode-line t)
  (spacious-padding-widths '(:internal-border-width 15
                             :right-divider-width 20
                             :header-line-width 4
                             :mode-line-width 1
                             :tab-width 3
                             :scroll-bar-width 8
                             :left-fringe-width 8
                             :right-fringe-width 13)))


(provide 'obsolete/me-spacious-padding)

;;; me-spacious-padding.el ends here
