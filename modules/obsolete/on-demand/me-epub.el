;;; me-epub.el --- EPUB support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-11-22

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-epub
  :auto-mode '(("\\.[eE][pP][uU][bB]\\'" . nov-mode)))


;; Featureful EPUB reader mode
(use-package nov
  :ensure t
  :mode ("\\.[eE][pP][uU][bB]\\'" . nov-mode))


(provide 'obsolete/on-demand/me-epub)
;;; me-epub.el ends here
