;;; me-whitespace-cleanup-mode.el --- Responsible whitespace cleanup (replaced by `ws-butler') -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package whitespace-cleanup-mode
  :straight t
  :hook (minemacs-first-file . global-whitespace-cleanup-mode)
  :custom
  (whitespace-cleanup-mode-preserve-point t))


(provide 'obsolete/me-whitespace-cleanup-mode)
;;; me-whitespace-cleanup-mode.el ends here
