;;; me-mathematica.el --- Wolfram Mathematica language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-mathematica) ; I prefer using Octave for ".m" files


;; Mathematica editing and inferior mode
(use-package wolfram-mode
  :straight (:host github :repo "kawabata/wolfram-mode"))


(provide 'on-demand/me-mathematica)
;;; me-mathematica.el ends here
