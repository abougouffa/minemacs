;;; me-epub.el --- EPUB support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-epub 'nov-mode
  :auto-mode '(("\\.[eE][pP][uU][bB]\\'" . nov-mode)))

(use-package nov
  :straight t
  :mode ("\\.[eE][pP][uU][bB]\\'" . nov-mode))


(provide 'on-demand/me-epub)
;;; me-epub.el ends here
