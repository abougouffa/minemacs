;;; me-scheme.el --- Scheme support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-scheme
  :auto-mode '(("\\.rkt[dl]?\\'" . racket-mode))
  :interpreter-mode '(("racket" . racket-mode))
  :companion-packages '(((racket-mode scheme-mode) . (geiser-mode flymake-guile))))


;; Racket editing, REPL, and more
(use-package racket-mode
  :ensure t)


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
