;;; me-cue.el --- CUE language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-22
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cue
  :auto-mode '(("\\.cue\\'" . cue-mode)))


;; Major mode for CUE language files
(use-package cue-mode
  :straight t)


(provide 'on-demand/me-cue)
;;; me-cue.el ends here
