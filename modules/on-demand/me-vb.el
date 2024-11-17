;;; me-vb.el --- Visual Basic language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-vb
  :auto-mode '(("\\.vbs?\\'" . visual-basic-mode)))


;; A mode for editing Visual Basic programs
(use-package visual-basic-mode
  :straight t
  :mode "\\.vbs?\\'"
  :hook (visual-basic-mode . +prog-mode-run-hooks))


(provide 'on-demand/me-vb)
;;; me-vb.el ends here
