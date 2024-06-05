;;; me-scheme.el --- Scheme packages                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

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


(provide 'me-scheme)
;;; me-scheme.el ends here
