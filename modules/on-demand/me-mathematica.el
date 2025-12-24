;;; me-mathematica.el --- Wolfram Mathematica language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mathematica) ; I prefer using Octave for ".m" files


;; Mathematica editing and inferior mode
(use-package wolfram-mode
  :vc (:url "https://github.com/kawabata/wolfram-mode"))


(provide 'on-demand/me-mathematica)
;;; me-mathematica.el ends here
