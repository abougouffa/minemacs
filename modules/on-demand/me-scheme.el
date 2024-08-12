;;; me-scheme.el --- Scheme support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-scheme
  :auto-mode '(("\\.rkt[dl]?\\'" . racket-mode))
  :interpreter-mode '(("racket" . racket-mode))
  :companion-packages '(((racket-mode scheme-mode) . geiser-mode)))

(use-package racket-mode
  :straight t)

(use-package geiser
  :straight t
  :custom
  (geiser-default-implementation 'guile))

(use-package geiser-chez
  :straight t)

(use-package geiser-guile
  :straight t)

(use-package geiser-mit
  :straight t)

(use-package geiser-racket
  :straight t)

(use-package macrostep-geiser
  :straight t
  :after geiser
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup))


(provide 'on-demand/me-scheme)
;;; me-scheme.el ends here
