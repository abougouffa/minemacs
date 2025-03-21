;;; me-csv.el --- CSV files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-csv
  :auto-mode '(("\\.[Cc][Ss][Vv]\\'" . csv-mode) ("\\.[Tt][Ss][Vv]\\'" . tsv-mode))
  :companion-packages '((csv-mode . rainbow-csv)))


;; Major mode for editing comma/char separated values
(use-package csv-mode
  :straight t
  :hook (csv-mode . csv-guess-set-separator)
  :custom
  (csv-separators '("," ";" "\t" "|")))


;; Highlight CSV and TSV files in different rainbow colors
(use-package rainbow-csv
  :straight (:host github :repo "emacs-vs/rainbow-csv"))


(provide 'on-demand/me-csv)
;;; me-csv.el ends here
