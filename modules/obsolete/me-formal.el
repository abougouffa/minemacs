;;; me-formal.el --- Formal verification tools and languages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package alloy-mode
  :straight (:host github :repo "dwwmmn/alloy-mode")
  :mode "\\.als\\'")

(use-package proof-general
  :straight t
  :init
  ;; `proof-general' sets `coq-mode' for ".v" files. However, I prefer use `verilog-mode' for ".v" files by default.
  (setq auto-mode-alist (delete (rassoc 'coq-mode auto-mode-alist) auto-mode-alist)))


(provide 'obsolete/me-formal)

;;; me-formal.el ends here
