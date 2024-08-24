;;; me-wiki.el --- Wiki editing -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-wiki
  :auto-mode '(("\\.wiki\\'" . wikitext-mode)))

(use-package wikitext-mode
  :straight t
  :mode "\\.wiki\\'")

(use-package mediawiki
  :straight t
  :commands (mediawiki-mode))


(provide 'on-demand/me-wiki)
;;; me-wiki.el ends here