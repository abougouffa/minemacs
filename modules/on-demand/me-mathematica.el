;;; me-mathematica.el --- Wolfram Mathematica language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mathematica) ; I prefer using Octave for ".m" files

(use-package wolfram-mode
  :straight (:host github :repo "kawabata/wolfram-mode"))


(provide 'on-demand/me-mathematica)
;;; me-mathematica.el ends here
