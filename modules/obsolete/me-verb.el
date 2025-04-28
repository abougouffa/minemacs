;;; me-verb.el --- Verb REST client -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-04-17

;;; Commentary:

;;; Code:


;; Organize and send HTTP requests from Emacs' Org mode files
(use-package verb
  :straight t
  :config
  (keymap-set org-mode-map "C-c C-r" `("verb" . ,verb-command-map)))


;; Import of Postman collections in Emacs (for `verb' and `restclient')
(use-package impostman
  :straight t)


(provide 'obsolete/me-verb)
;;; me-verb.el ends here
