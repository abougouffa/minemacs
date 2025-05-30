;;; me-scheme.el --- Scheme support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2023-07-31
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-scheme
  :auto-mode '(("\\.rkt[dl]?\\'" . racket-mode))
  :interpreter-mode '(("racket" . racket-mode))
  :companion-packages '(((racket-mode scheme-mode) . (geiser-mode flymake-guile))))


;; Racket editing, REPL, and more
(use-package racket-mode
  :straight t)


;; Generic Scheme interaction mode with an enhanced REPL and a set of minor modes
(use-package geiser
  :straight t
  :custom
  (geiser-default-implementation 'guile))


;; Chez Scheme and Geiser talk to each other
(use-package geiser-chez
  :straight t)


;; Guile Scheme and Geiser talk to each other
(use-package geiser-guile
  :straight t)


;; MIT Scheme and Geiser talk to each other
(use-package geiser-mit
  :straight t)


;; Racket Scheme and Geiser talk to each other
(use-package geiser-racket
  :straight t)


;; Macrostep for `geiser'
(use-package macrostep-geiser
  :straight t
  :after geiser
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup))


;; Guile `flymake' backend
(use-package flymake-guile
  :straight (:source emacsmirror-mirror)
  :init
  (when (executable-find "guild") (add-hook 'scheme-mode-hook #'flymake-guile)))


(provide 'on-demand/me-scheme)
;;; me-scheme.el ends here
