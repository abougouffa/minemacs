;;; me-expand-region.el --- expand-region config for non-treesitter build -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package expand-region
  :straight t
  :init
  (+vmap!
    "v" #'er/expand-region
    "q" #'er/contract-region))

(provide 'obsolete/me-expand-region)

;;; me-expand-region.el ends here
