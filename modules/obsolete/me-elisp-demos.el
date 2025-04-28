;;; me-elisp-demos.el --- elisp-demos -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-04-11
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

(use-package elisp-demos
  :straight t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))


(provide 'obsolete/me-elisp-demos)
;;; me-elisp-demos.el ends here
