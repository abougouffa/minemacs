;;; me-solaire.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-16
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Visually distinguish "real" buffers (i.e. file-visiting code buffers) from "unreal" buffers (like popups, sidebars, etc)
(use-package solaire-mode
  :ensure t
  :hook (minemacs-after-startup . solaire-global-mode))


(provide 'obsolete/me-solaire)
;;; me-solaire.el ends here
