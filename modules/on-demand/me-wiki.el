;;; me-wiki.el --- Wiki editing -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-23
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-wiki
  :auto-mode '(("\\.\\(media\\)?wiki\\'" . mediawiki-file-mode)))


;; MediaWiki frontend
(use-package mediawiki
  :ensure t
  :commands (mediawiki-mode mediawiki-file-mode))


(provide 'on-demand/me-wiki)
;;; me-wiki.el ends here
