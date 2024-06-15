;;; me-better-jumper.el --- Better jumper integration (needs more hooks) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package better-jumper
  :straight t
  :hook (minemacs-lazy . better-jumper-mode)
  ;; Map extra mouse buttons to jump forward/backward
  :bind (("<mouse-8>" . better-jumper-jump-backward)
         ("<mouse-9>" . better-jumper-jump-forward)))


(provide 'obsolete/me-better-jumper)
;;; me-better-jumper.el ends here
