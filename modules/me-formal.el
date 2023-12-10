;;; me-formal.el --- Formal verification tools and languages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

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


(provide 'me-formal)

;;; me-formal.el ends here
