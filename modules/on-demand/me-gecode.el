;;; me-gecode.el --- G-Code -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-gecode
  :auto-mode '((("\\.gco\\(?:de\\)?\\'" "\\.GCO\\(?:DE\\)?\\'" "\\.[nN][gG]?[cC]\\'" "\\.mp[tf]\\'") . gcode-mode)))


;; Simple G-Code major mode
(use-package gcode-mode
  :straight t
  :hook (gcode-mode . eldoc-mode))


(provide 'on-demand/me-gecode)
;;; me-gecode.el ends here
