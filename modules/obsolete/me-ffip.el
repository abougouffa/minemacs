;;; me-ffip.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-16
;; Last modified: 2025-07-16

;;; Commentary:

;;; Code:


;; Quick access to project files using `fd'
(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t)))


(provide 'obsolete/me-ffip)
;;; me-ffip.el ends here
