;;; me-isearch-mb.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Control `isearch' from the minibuffer
(use-package isearch-mb
  :straight t
  :after isearch
  :init
  (isearch-mb-mode 1))


(provide 'obsolete/me-isearch-mb)
;;; me-isearch-mb.el ends here
