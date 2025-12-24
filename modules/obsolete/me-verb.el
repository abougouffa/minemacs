;;; me-verb.el --- Verb REST client -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-04-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Organize and send HTTP requests from Emacs' Org mode files
(use-package verb
  :ensure t
  :config
  (keymap-set org-mode-map "C-c C-r" `("verb" . ,verb-command-map)))


;; Import of Postman collections in Emacs (for `verb' and `restclient')
(use-package impostman
  :ensure t)


(provide 'obsolete/me-verb)
;;; me-verb.el ends here
