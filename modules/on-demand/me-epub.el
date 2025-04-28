;;; me-epub.el --- EPUB support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-epub
  :auto-mode '(("\\.[eE][pP][uU][bB]\\'" . nov-mode)))


;; Featureful EPUB reader mode
(use-package nov
  :straight t
  :mode ("\\.[eE][pP][uU][bB]\\'" . nov-mode))


(provide 'on-demand/me-epub)
;;; me-epub.el ends here
