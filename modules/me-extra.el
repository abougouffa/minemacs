;;; me-extra.el --- Some extra functionalities -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

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
         ("C-x 4 t" . crux-transpose-windows)
         :map minemacs-open-thing-map
         ("o" . crux-open-with)))


;; Override the `default-directory' in the next command
(use-package run-in-dir
  :straight (:host github :repo "abougouffa/run-in-dir")
  :bind (("C-c C-i" . run-in-dir-prefix)))


(provide 'me-extra)

;;; me-extra.el ends here
