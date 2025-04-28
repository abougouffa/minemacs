;;; me-formal.el --- Formal verification tools and languages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-11-18
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package proof-general
  :straight t
  :init
  ;; `proof-general' sets `coq-mode' for ".v" files. However, I prefer use `verilog-mode' for ".v" files by default.
  (setq auto-mode-alist (delete (rassoc 'coq-mode auto-mode-alist) auto-mode-alist)))


(provide 'obsolete/me-formal)
;;; me-formal.el ends here
