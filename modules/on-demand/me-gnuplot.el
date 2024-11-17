;;; me-gnuplot.el --- GNU Plot -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-gnuplot
  :auto-mode '((("\\.plot\\'" "\\.gpi\\'" "\\.gplt\\'" "\\.plt\\'" "\\.gnuplot\\'") . gnuplot-mode)))


;; Major mode and interactive frontend for GNUPlot
(use-package gnuplot
  :straight t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :mode ("\\.plt\\'" . gnuplot-mode)
  :mode ("\\.gplt\\'" . gnuplot-mode)
  :mode ("\\.gpi\\'" . gnuplot-mode)
  :mode ("\\.plot\\'" . gnuplot-mode)
  :hook (gnuplot-mode . +prog-mode-run-hooks)
  :hook (gnuplot-mode . visual-line-mode))


(provide 'on-demand/me-gnuplot)
;;; me-gnuplot.el ends here
