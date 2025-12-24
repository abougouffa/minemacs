;;; me-isearch-mb.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-11-21
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Control `isearch' from the minibuffer
(use-package isearch-mb
  :ensure t
  :after isearch
  :init
  (isearch-mb-mode 1))


(provide 'obsolete/me-isearch-mb)
;;; me-isearch-mb.el ends here
