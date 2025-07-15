;;; me-project.el --- Projects stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-07-15

;;; Commentary:

;;; Code:

;; One tab per project, with unique names - simple implementation of workspaces
(use-package otpp
  :straight (:host github :repo "abougouffa/one-tab-per-project")
  :after project
  :bind (("C-x t D" . otpp-detach-buffer-to-tab)
         ("C-x t C" . otpp-change-tab-root-dir)
         ("C-x t P" . otpp-prefix))
  :custom
  (otpp-project-aware-commands-regexp (rx (seq bol (or "project-" "+project-" "projection-"))))
  :init
  (otpp-mode 1)
  (otpp-override-mode 1))


;; Multi target interface to compile
(use-package compile-multi
  :straight t
  :bind (("<f9>" . compile-multi)))


;; Integration for `compile-multi' and `embark'
(use-package compile-multi-embark
  :straight t
  :after embark
  :init
  (compile-multi-embark-mode 1))


;; Consulting read support for `compile-multi'
(use-package consult-compile-multi
  :straight t
  :unless (+package-disabled-p 'consult 'me-completion)
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
  :bind (:map projection-map ("f" . projection-find-other-file))
  :init
  ;; BUG: `projection-hook' causes Tramp issues, don't enable `global-projection-hook-mode'
  ;; This ensures that `ibuffer-projection-set-filter-groups' takes effect
  (add-hook 'ibuffer-hook (lambda () (run-at-time 0.1 nil (lambda () (call-interactively #'ibuffer-update)))))
  ;; Mark compile commands as safe (customized in ".dir-locals.el")
  (dolist (var '( projection-commands-configure-command projection-commands-build-command
                  projection-commands-test-command projection-commands-run-command
                  projection-commands-package-command projection-commands-install-command))
    (put var 'safe-local-variable #'stringp)))


;; Projection extension to jump between related files in a project
(use-package projection-find
  :config
  ;; Add header/source mapping for Modula-2
  (cl-callf2 append '(("mod" "def") ("def" "mod")) projection-find-other-file-suffix))


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


;; Quick access to project files using `fd'
(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t)))


(provide 'me-project)

;;; me-project.el ends here
