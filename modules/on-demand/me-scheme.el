;;; me-scheme.el --- Scheme support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-07-31
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-scheme
  :companion-packages '(((racket-mode scheme-mode) . (geiser-mode flymake-guile))))


;; Generic Scheme interaction mode with an enhanced REPL and a set of minor modes
(use-package geiser
  :ensure t
  :custom
  (geiser-default-implementation 'guile))


;; Chez Scheme and Geiser talk to each other
(use-package geiser-chez
  :ensure t)


;; Guile Scheme and Geiser talk to each other
(use-package geiser-guile
  :ensure t)


;; MIT Scheme and Geiser talk to each other
(use-package geiser-mit
  :ensure t)


;; Racket Scheme and Geiser talk to each other
(use-package geiser-racket
  :ensure t)


;; Macrostep for `geiser'
(use-package macrostep-geiser
  :ensure t
  :after geiser
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup))


;; Guile `flymake' backend
(use-package flymake-guile
  :ensure t
  :init
  (when (executable-find "guild") (add-hook 'scheme-mode-hook #'flymake-guile)))


(provide 'on-demand/me-scheme)
;;; me-scheme.el ends here
