;;; me-org-rich-yank.el --- Org rich yank -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-02-23
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Rich text clipboard for org-mode: Paste as a #+BEGIN_SRC block of correct mode, with link to where it came from
(use-package org-rich-yank
  :ensure t
  :hook (minemacs-lazy . org-rich-yank-enable))


(provide 'obsolete/me-org-rich-yank)
;;; me-org-rich-yank.el ends here
