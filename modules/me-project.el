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

(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t)))

(use-package compile-multi
  :straight t)

(use-package compile-multi-embark
  :straight t
  :after embark
  :init
  (compile-multi-embark-mode 1))

(use-package consult-compile-multi
  :straight t
  :after consult
  :init
  (consult-compile-multi-mode 1))

(use-package projection
  :straight t
  :hook (ibuffer . ibuffer-projection-set-filter-groups)
  :bind-keymap ("C-x P" . projection-map)
  :init
  ;; This ensures that `ibuffer-projection-set-filter-groups' takes effect
  (+add-hook! ibuffer (run-at-time 0.1 nil (lambda () (call-interactively #'ibuffer-update)))))

(use-package projection-multi
  :straight t
  :init
  (+map! "pC" #'projection-multi-compile))

(use-package projection-multi-embark
  :straight t
  :after embark projection-multi
  :init
  (projection-multi-embark-setup-command-map))

(use-package projection-dape
  :straight t
  :init
  (+map! :infix "d"
    "D" #'projection-dape))

(use-package project-x
  :straight (:host github :repo "karthink/project-x")
  :after project
  :commands project-x-window-state-save project-x-window-state-load
  :custom
  (project-x-window-list-file (concat minemacs-local-dir "project-x/project-window-list.el"))
  (project-x-local-identifier '("project.el" ".project.el"))
  :init
  (project-x-mode 1))


(provide 'me-project)

;;; me-project.el ends here
