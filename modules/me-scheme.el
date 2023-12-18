;;; me-scheme.el --- Scheme packages                 -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package racket-mode
  :elpaca t)

(use-package geiser
  :elpaca t
  :custom
  (geiser-default-implementation 'guile))

(use-package geiser-chez
  :elpaca t)

(use-package geiser-guile
  :elpaca t)

(use-package geiser-mit
  :elpaca t)

(use-package geiser-racket
  :elpaca t)

(use-package macrostep-geiser
  :elpaca t
  :after geiser
  :hook ((geiser-mode geiser-repl-mode) . macrostep-geiser-setup)
  :init
  (+map-local! :keymaps '(geiser-mode-map geiser-repl-mode-map)
    "m" '(macrostep-expand :wk "Expand macro")
    "M" #'macrostep-geiser-expand-all))


(provide 'me-scheme)
;;; me-scheme.el ends here
