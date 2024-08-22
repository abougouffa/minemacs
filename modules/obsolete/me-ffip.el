;;; me-ffip.el --- description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t)))


(provide 'obsolete/me-ffip)
;;; me-ffip.el ends here
