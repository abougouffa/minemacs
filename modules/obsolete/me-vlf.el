;;; me-vlf.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-08-07
;; Last modified: 2025-08-07

;;; Commentary:

;;; Code:


;; View, edit, search and compare very large files in batches, trading memory for processor time
(use-package vlf-setup
  :straight (vlf :source gnu-elpa-mirror)
  :demand
  :config
  (with-eval-after-load 'so-long
    (add-to-list 'so-long-mode-preserved-variables 'vlf-mode)))


(provide 'obsolete/me-vlf)
;;; me-vlf.el ends here
