;;; me-org-rich-yank.el --- Org rich yank -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; Rich text clipboard for org-mode: Paste as a #+BEGIN_SRC block of correct mode, with link to where it came from
(use-package org-rich-yank
  :straight t
  :hook (minemacs-lazy . org-rich-yank-enable))


(provide 'obsolete/me-org-rich-yank)
;;; me-org-rich-yank.el ends here
