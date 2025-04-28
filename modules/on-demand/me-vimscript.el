;;; me-vimscript.el --- VimScript support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-vimscript
  :auto-mode '((("\\.vim\\'" "[._]?g?vimrc\\'" "\\.exrc\\'") . vimrc-mode)))


;; Major mode for vimrc files
(use-package vimrc-mode
  :straight t)


(provide 'on-demand/me-vimscript)
;;; me-vimscript.el ends here
