;;; me-phi-search.el --- phi-search & phi-grep -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Another incremental search command, compatible with `multiple-cursors'
(use-package phi-search
  :straight t)


;; Interactively-editable recursive "grep" implementation in Elisp
(use-package phi-grep
  :straight t)


(provide 'obsolete/me-phi-search)
;;; me-phi-search.el ends here
