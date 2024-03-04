;;; me-solaire.el --- Solaire -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package solaire-mode
  :straight t
  :hook (minemacs-after-startup . solaire-global-mode)
  :config
  (dolist (face '(mode-line mode-line-active mode-line-inactive mode-line-emphasis))
    (setf (alist-get face solaire-mode-remap-alist) nil)))


(provide 'obsolete/me-solaire)

;;; me-solaire.el ends here
