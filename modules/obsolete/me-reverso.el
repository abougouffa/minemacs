;;; me-reverso.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-12-11
;; Last modified: 2025-12-11

;;; Commentary:

;;; Code:


;; Emacs client for Reverso.net for translation, grammar check, context and synonyms search
(use-package reverso
  :straight (:host github :repo "SqrtMinusOne/reverso.el")
  :bind (:map minemacs-open-thing-map ("r" . reverso))
  :config
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'reverso--history))
  (reverso-history-mode 1))


(provide 'obsolete/me-reverso)
;;; me-reverso.el ends here
