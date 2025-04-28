;;; me-statistics.el --- Statistics using R and Julia -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-04-20

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-statistics
  :auto-mode '((("\\(NAMESPACE\\|CITATION\\)\\'" "/R/.*\\.q\\'" "\\.[rR]\\(profile\\)?\\'") . ess-r-mode)
               (("\\.[Bb][Uu][Gg]\\'" "\\.[Bb][Oo][Gg]\\'" "\\.[Bb][Mm][Dd]\\'") . ess-bugs-mode)
               ("\\.[Rr]out\\'" . ess-r-transcript-mode)
               ("\\.Rd\\'" . Rd-mode)
               ("\\.[Jj][Aa][Gg]\\'" . ess-jags-mode)
               ("\\.inp\\'" . ess-gretl-mode)
               ("\\.jl\\'" . julia-mode))
  :interpreter-mode '((("r" "Rscript") . ess-r-mode)
                      ("julia" . julia-mode)))


;;;###autoload(add-to-list 'auto-mode-alist '("/Makevars\\(\\.win\\)?\\'" . makefile-mode))
;;;###autoload(add-to-list 'auto-mode-alist '("DESCRIPTION\\'" . conf-colon-mode))


;; Julia support in Emacs
(use-package julia-mode
  :straight t)


;; Emacs Speaks Statistics
(use-package ess
  :straight t)


;; View R dataframes in a spreadsheet software
(use-package ess-view
  :straight t)


;; Data viewer for GNU R
(use-package ess-R-data-view
  :straight t)


(provide 'on-demand/me-statistics)
;;; me-statistics.el ends here
