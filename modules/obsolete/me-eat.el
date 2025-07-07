;;; me-eat.el --- EAT -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-07
;; Last modified: 2025-07-07

;;; Commentary:

;;; Code:


;; Emulate A Terminal, in a region, in a buffer and in Eshell
(use-package eat
  :straight t
  :hook (eat-mode . minemacs-reduce-font-size)
  :config
  (advice-add 'eat--sentinel :around #'+kill-buffer-after-sentinel-exit))


(provide 'obsolete/me-eat)
;;; me-eat.el ends here
