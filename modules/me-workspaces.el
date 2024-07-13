;;; workspaces.el --- Windows, workspaces (via tab-bar & tab-line) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;; IDEA:
;; - github.com/fritzgrabo/project-tab-groups
;; - github.com/florommel/bufferlo
;; - www.rousette.org.uk/archives/using-the-tab-bar-in-emacs

;;; Code:

(use-package unique-dir-name
  :straight (:host github :repo "abougouffa/unique-dir-name"))

(use-package one-tab-per-project
  :straight (:host github :repo "abougouffa/one-tab-per-project")
  :after project
  :hook (otpp-after-define-commands . +one-tab-per-project--tweak-consult-cmds-h)
  :init
  (otpp-mode 1)
  (otpp-remap-commands-mode 1)
  :config
  (defun +one-tab-per-project--tweak-consult-cmds-h ()
    (with-eval-after-load 'consult
      (consult-customize
       otpp-consult-fd :initial (+region-or-thing-at-point)
       otpp-consult-find :initial (+region-or-thing-at-point)
       otpp-consult-grep :initial (+region-or-thing-at-point)
       otpp-consult-ripgrep :initial (+region-or-thing-at-point)))))

(use-package burly
  :straight t)

(use-package bufler
  :straight t)


(provide 'me-workspaces)

;;; me-workspaces.el ends here
