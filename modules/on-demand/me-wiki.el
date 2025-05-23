;;; me-wiki.el --- Wiki editing -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-23
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-wiki
  :auto-mode '(("\\.wiki\\'" . wikitext-mode)))


;; Major mode for editing Wikitexts
(use-package wikitext-mode
  :straight t
  :mode "\\.wiki\\'")


;; MediaWiki frontend
(use-package mediawiki
  :straight t
  :commands (mediawiki-mode))


(provide 'on-demand/me-wiki)
;;; me-wiki.el ends here
