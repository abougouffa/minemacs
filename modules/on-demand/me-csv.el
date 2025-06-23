;;; me-csv.el --- CSV files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-06-23

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-csv
  :auto-mode '(("\\.[Cc][Ss][Vv]\\'" . csv-mode) ("\\.[Tt][Ss][Vv]\\'" . tsv-mode))
  :companion-packages '((csv-mode . (rainbow-csv eplot))))


;; Major mode for editing comma/char separated values
(use-package csv-mode
  :straight t
  :hook (csv-mode . csv-guess-set-separator)
  :custom
  (csv-separators '("," ";" "\t" "|" " ")))


;; Highlight CSV and TSV files in different rainbow colors
(use-package rainbow-csv
  :straight (:host github :repo "emacs-vs/rainbow-csv")
  :hook (csv-mode . +rainbow-csv-mode-maybe)
  :config
  (defvar +rainbow-csv-max-file-size (* 10 1024 1024)) ; 10MB
  (defun +rainbow-csv-mode-maybe ()
    (when (< (buffer-size) +rainbow-csv-max-file-size)
      (run-with-timer 0.1 nil #'rainbow-csv-mode))))


;; Interactively generate time series charts, plots and bar charts
(use-package eplot
  :straight (:host github :repo "larsmagne/eplot")
  :commands (eplot eplot-with-headers))


(provide 'on-demand/me-csv)
;;; me-csv.el ends here
