;;; me-cue.el --- CUE language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cue
  :auto-mode '(("\\.cue\\'" . cue-mode)))


;; Major mode for CUE language files
(use-package cue-mode
  :ensure t)


(provide 'on-demand/me-cue)
;;; me-cue.el ends here
