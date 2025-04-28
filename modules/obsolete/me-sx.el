;;; me-sx.el --- StackExchange -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-04-19
;; Last modified: 2025-04-19

;;; Commentary:

;;; Code:


;; Stack Exchange for Emacs
(use-package sx
  :straight t
  :custom
  (sx-cache-directory (concat minemacs-cache-dir "sx/")))


(provide 'obsolete/me-sx)
;;; me-sx.el ends here
