;;; me-gnuplot.el --- GNU Plot -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-gnuplot 'gnuplot-mode
  :auto-mode '((("\\.plot\\'" "\\.gpi\\'" "\\.gplt\\'" "\\.plt\\'" "\\.gnuplot\\'") . gnuplot-mode)))

(use-package gnuplot
  :straight t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :mode ("\\.plt\\'" . gnuplot-mode)
  :mode ("\\.gplt\\'" . gnuplot-mode)
  :mode ("\\.gpi\\'" . gnuplot-mode)
  :mode ("\\.plot\\'" . gnuplot-mode)
  :hook (gnuplot-mode . display-line-numbers-mode)
  :hook (gnuplot-mode . visual-line-mode))


(provide 'on-demand/me-gnuplot)
;;; me-gnuplot.el ends here
