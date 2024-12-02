;;; me-wiki.el --- Wiki editing -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-wiki
  :auto-mode '(("\\.wiki\\'" . wikitext-mode)))


;; Major mode for editing Wikitexts
(use-package wikitext-mode
  :vc (:url "https://github.com/emacsmirror/wikitext-mode")
  :mode "\\.wiki\\'")


;; MediaWiki frontend
(use-package mediawiki
  :ensure t
  :commands (mediawiki-mode))


(provide 'on-demand/me-wiki)
;;; me-wiki.el ends here
