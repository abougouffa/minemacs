;;; me-easysession.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-12-11
;; Last modified: 2025-12-11

;;; Commentary:

;;; Code:


;; Effortlessly persist and restore your Emacs sessions
(use-package easysession
  :straight t
  :hook (minemacs-lazy . easysession-save-mode))


(provide 'obsolete/me-easysession)
;;; me-easysession.el ends here
