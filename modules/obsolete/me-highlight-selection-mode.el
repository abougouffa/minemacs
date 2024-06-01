;;; me-selection-highlight-mode.el --- Code coverage -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package selection-highlight-mode
  :straight (:host github :repo "balloneij/selection-highlight-mode")
  :hook (minemacs-lazy . selection-highlight-mode))


(provide 'obsolete/me-selection-highlight-mode)
;;; me-selection-highlight-mode.el ends here
