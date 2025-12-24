;;; me-code-cells.el --- code-cells -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-30
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Lightweight notebooks in Emacs
(use-package code-cells
  :ensure t
  :init
  ;; Both `ein' and `code-cells' registers auto-mode for ".ipynb" files,
  ;; we remove `code-cells' so `ein' gets used by default.
  (unless (+package-disabled-p 'ein)
    (setq auto-mode-alist (delete (rassoc 'code-cells-convert-ipynb auto-mode-alist) auto-mode-alist))))


(provide 'obsolete/me-code-cells)
;;; me-code-cells.el ends here
