;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-12-26
;; Last modified: 2025-12-09

;;; Commentary:

;;; Code:

;; A Collection of Ridiculously Useful eXtensions for Emacs
(use-package crux
  :straight t
  :bind (("C-k" . crux-smart-kill-line)
         ("C-<return>" . crux-smart-open-line)
         ("C-S-<return>" . crux-smart-open-line-above)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c u" . crux-view-url)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("C-x 4 t" . crux-transpose-windows)))


;; Persistent per-project scratch buffers for Emacs
(use-package pscratch
  :straight (:host github :repo "abougouffa/persistent-scratch")
  :hook (minemacs-after-startup . pscratch-mode))


(provide 'me-extra)

;;; me-extra.el ends here
