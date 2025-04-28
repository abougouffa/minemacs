;;; me-smalltalk.el --- SmallTalk -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-smalltalk
  :auto-mode '(("\\.st\\'" . smalltalk-mode)))


(use-package smalltalk-mode
  :straight t)


(provide 'on-demand/me-smalltalk)
;;; me-smalltalk.el ends here
