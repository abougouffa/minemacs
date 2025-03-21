;;; me-highlight-indent-guides.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package highlight-indent-guides
  :straight t
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-character #x2506)
  (highlight-indent-guides-responsive 'top))


(provide 'obsolete/me-highlight-indent-guides)
;;; me-highlight-indent-guides.el ends here
