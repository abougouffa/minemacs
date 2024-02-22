;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package consult-project-extra
  :straight t
  :init
  (+map! :infix "p" ;; project
    "p" #'consult-project-extra-find
    "P" #'consult-project-extra-find-other-window))

(use-package ibuffer-project
  :straight t
  :hook (ibuffer . +ibuffer-project-h)
  :config
  ;; From Crafted Emacs
  (defun +ibuffer-project-h ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative))))

(use-package projection
  :straight t
  ;; Enable the `projection-hook' feature.
  :hook (minemacs-after-startup . global-projection-hook-mode)
  :config
  (with-eval-after-load 'project (require 'projection))
  ;; Access pre-configured projection commands from a keybinding of your choice.
  ;; Run `M-x describe-keymap projection-map` for a list of available commands.
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi
  :straight t
  :init
  (+map! "pC" #'projection-multi-compile))

(use-package projection-multi-embark
  :straight t
  :after embark projection-multi
  :demand t
  ;; Add the projection set-command bindings to `compile-multi-embark-command-map'.
  :config
  (projection-multi-embark-setup-command-map))


(provide 'me-project)

;;; me-project.el ends here
