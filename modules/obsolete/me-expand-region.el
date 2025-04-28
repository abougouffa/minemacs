;;; me-expand-region.el --- expand-region config for non-treesitter build -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-09-04
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package expand-region
  :straight t
  :bind (("C-M-SPC" . er/expand-region) ; orig. `mark-sexp'
         ("S-C-M-SPC" . er/contract-region)))


(provide 'obsolete/me-expand-region)
;;; me-expand-region.el ends here
