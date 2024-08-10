;;; me-vimrc.el --- Vimrc support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-vimrc 'vimrc-mode
  :auto-mode '((("\\.vim\\'" "[._]?g?vimrc\\'" "\\.exrc\\'") . vimrc-mode)))

(use-package vimrc-mode
  :straight t)


(provide 'modes/me-vimrc)
;;; me-vimrc.el ends here
