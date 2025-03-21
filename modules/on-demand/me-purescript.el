;;; me-purescript.el --- PureScript language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-purescript
  :auto-mode '(("\\.purs\\'" . purescript-mode)))


;; A PureScript editing mode
(use-package purescript-mode
  :straight t)


(provide 'on-demand/me-purescript)
;;; me-purescript.el ends here
