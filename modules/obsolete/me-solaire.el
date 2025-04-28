;;; me-solaire.el --- Solaire -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package solaire-mode
  :straight t
  :hook (minemacs-lazy . solaire-global-mode)
  :config
  (dolist (face '(mode-line mode-line-active mode-line-inactive mode-line-emphasis))
    (setf (alist-get face solaire-mode-remap-alist) nil)))


(provide 'obsolete/me-solaire)
;;; me-solaire.el ends here
