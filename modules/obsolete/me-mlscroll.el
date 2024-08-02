;;; me-mlscroll.el --- Mode line scroll bar -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package mlscroll
  :straight (:host github :repo "jdtsmith/mlscroll")
  :hook (minemacs-lazy . mlscroll-mode))


(provide 'obsolete/me-mlscroll)
;;; me-mlscroll.el ends here
