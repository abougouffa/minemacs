;;; me-mathematica.el --- Wolfram Mathematica language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mathematica) ; I prefer using Octave for ".m" files


;; Mathematica editing and inferior mode
(use-package wolfram-mode
  :vc (:url "https://github.com/kawabata/wolfram-mode"))


(provide 'on-demand/me-mathematica)
;;; me-mathematica.el ends here
