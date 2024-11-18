;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; One tab per project, with unique names - simple implementation of workspaces
(use-package otpp
  :straight (:host github :repo "abougouffa/one-tab-per-project")
  :after project
  :bind (("C-x t D" . otpp-detach-buffer-to-tab)
         ("C-x t C" . otpp-change-tab-root-dir)
         ("C-x t P" . otpp-prefix))
  :init
  (otpp-mode 1)
  (otpp-override-mode 1))


;; Consult extension for `project'
(use-package consult-project-extra
  :straight t)


;; Multi target interface to compile
(use-package compile-multi
  :straight t)


;; Integration for `compile-multi' and `embark'
(use-package compile-multi-embark
  :straight t
  :after embark
  :init
  (compile-multi-embark-mode 1))


;; Consulting read support for `compile-multi'
(use-package consult-compile-multi
  :straight t
  :after consult
  :init
  (consult-compile-multi-mode 1))


;; Integration of `compile-multi' with `nerd-icons'
(use-package compile-multi-nerd-icons
  :straight t
  :after compile-multi
  :demand t)


;; Projectile like project management library built on Emacs' `project'
(use-package projection
  :straight t
  :hook (ibuffer . ibuffer-projection-set-filter-groups)
  :after project
  :demand
  :bind-keymap ("C-x P" . projection-map)
  :init
  ;; This ensures that `ibuffer-projection-set-filter-groups' takes effect
  (add-hook 'ibuffer-hook (lambda () (run-at-time 0.1 nil (lambda () (call-interactively #'ibuffer-update)))))
  ;; Mark compile commands as safe (customized in ".dir-locals.el")
  (dolist (var '(projection-commands-configure-project projection-commands-build-project
                 projection-commands-test-project projection-commands-run-project
                 projection-commands-package-project projection-commands-install-project))
    (put var 'safe-local-variable #'stringp))
  ;; Enable `projection-hook', adds the possibility to run functions in per-project basis
  (global-projection-hook-mode 1))


;; Projection integration for `compile-multi'
(use-package projection-multi
  :straight t
  :bind (:map projection-map ("C" . #'projection-multi-compile)))


;; Integration for `projection-multi' and `embark'
(use-package projection-multi-embark
  :straight t
  :after embark
  :init
  (projection-multi-embark-setup-command-map))


;; Projection integration for `dape'
(use-package projection-dape
  :straight t
  :bind (:map projection-map ("D" . #'projection-dape)))


(provide 'me-project)

;;; me-project.el ends here
