;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package better-jumper
  :straight t
  :hook (minemacs-lazy . better-jumper-mode)
  ;; Map extra mouse buttons to jump forward/backward
  :bind (("<mouse-8>" . better-jumper-jump-backward)
         ("<mouse-9>" . better-jumper-jump-forward)))

(use-package dogears
  :straight t
  :hook (minemacs-lazy . dogears-mode)
  ;; These bindings are optional, of course:
  :bind (("M-g d" . dogears-go)
         ("M-g M-b" . dogears-back)
         ("M-g M-f" . dogears-forward)
         ("M-g M-d" . dogears-list)
         ("M-g M-D" . dogears-sidebar)))

(use-package crux
  :straight t
  :bind (("C-c o o" . crux-open-with)
         ("C-k" . crux-smart-kill-line)
         ("C-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c u" . crux-view-url)
         ("C-c 4 t" . crux-transpose-windows)))


(provide 'me-extra)

;;; me-extra.el ends here
