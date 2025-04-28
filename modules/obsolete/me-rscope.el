;;; me-rscope.el --- Reborn Cscope integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-09
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;; Reborn Cscope extension for Emacs
(use-package rscope
  :straight (:host github :repo "rjarzmik/rscope")
  :commands (rscope-init rscope-regenerate-database))


(provide 'obsolete/me-rscope)
;;; me-rscope.el ends here
