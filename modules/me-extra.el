;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; A Collection of Ridiculously Useful eXtensions for Emacs
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
