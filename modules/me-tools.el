;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-08-30

;;; Commentary:

;;; Code:

;; A deployment plugin via Tramp for Emacs
(use-package ssh-deploy
  :straight (ssh-deploy :source gnu-elpa-mirror)
  ;; Should be configured in per-project basis, good documentation at:
  ;; https://github.com/emacs-straight/ssh-deploy#deployment-configuration-examples
  :hook
  (after-save . ssh-deploy-after-save)
  (find-file . ssh-deploy-find-file)
  :bind (("C-c C-z" . ssh-deploy-prefix-map))
  :custom
  (ssh-deploy-revision-folder (concat minemacs-cache-dir "ssh-deploy-revisions/")))


;; TRAMP integration for Incus containers
(use-package incus-tramp
  :straight t
  :after tramp
  :init
  (incus-tramp-add-method))


;; TRAMP integration for LXC containers
(use-package lxc-tramp
  :straight t)


;; TRAMP integration for LXD containers
(use-package lxd-tramp
  :straight t)


;; Emulate A Terminal, in a region, in a buffer and in Eshell
(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :hook (eat-mode . minemacs-reduce-font-size)
  :config
  (advice-add 'eat--sentinel :around #'+kill-buffer-after-sentinel-exit))


;; Launch system applications from Emacs
(use-package app-launcher
  :straight (:host github :repo "SebastienWae/app-launcher")
  :when (or (featurep 'os/linux) (featurep 'os/bsd))
  :bind (:map minemacs-open-thing-map ("a" . app-launcher-run-app)))


;; Manage docker from Emacs
(use-package docker
  :straight t
  :bind (:map minemacs-open-thing-map ("d" . docker)))


;; Rudimentary devcontainer support for Emacs
(use-package devcontainer
  :straight (:host github :repo "johannes-mueller/devcontainer.el"))


;; Major mode to view journalctl's output in Emacs
(use-package journalctl-mode
  :straight t
  :commands (journalctl-mode))


;; Use the Emacsclient as the "$EDITOR" of child processes
(use-package with-editor
  :straight t
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . +with-editor-export-all)
  :init
  (once-x-call '(:before shell-command) #'shell-command-with-editor-mode)
  ;; `julia-repl' seems to start on `term-mode', so let's check for it before exporting the editor
  (defvar +with-editor-ignore-matching-buffers '("\\*julia\\*"))
  (defun +with-editor-export-all ()
    (unless (seq-some (+apply-partially-right #'string-match-p (buffer-name)) +with-editor-ignore-matching-buffers)
      (+shutup! ; Export "EDITOR", then "(HG|GIT|JJ)_EDITOR" when needed
       (with-editor-export-editor)
       (when (getenv "HG_EDITOR") (with-editor-export-hg-editor))
       (when (getenv "GIT_EDITOR") (with-editor-export-git-editor))
       (when (getenv "JJ_EDITOR") (with-editor-export-editor "JJ_EDITOR")))))
  :config
  (add-to-list 'with-editor-envvars "JJ_EDITOR")) ; Add support for Jujutsu (`jj')


;; Buffer-local "direnv" integration for Emacs
(use-package envrc
  :straight t
  :hook (minemacs-first-file . envrc-global-mode)
  :when (and (not (featurep 'os/win)) (executable-find "direnv"))
  :custom
  (envrc-debug minemacs-debug-p)
  (envrc-remote t)
  (envrc-supported-tramp-methods '("ssh" "docker"))
  :config
  (keymap-set minemacs-open-thing-map "v" envrc-command-map)
  (with-eval-after-load 'ob ; Ensure loading envrc for babel source blocks
    (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)))


;; Emacs integration for "pyenv"
(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :hook (minemacs-first-file . global-pyenv-mode)
  :custom
  (pyenv-show-active-python-in-modeline nil)
  :init
  (when-let* ((exe (executable-find "pyenv")))
    (setq pyenv-executable exe)) ; In some cases, pyenv is installed under "/usr/bin/pyenv"
  (advice-add 'pyenv-use :around #'+apply-suppress-messages))


;; Adds the "node_modules/.bin" directory to the buffer "exec_path"
(use-package add-node-modules-path
  :straight t
  :hook (js-base-mode . add-node-modules-path)
  :config
  (when (executable-find "pnpm")
    (setopt add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))))


;; Mount/umount eCryptfs private directory from Emacs
(use-package ecryptfs
  :straight (:host github :repo "abougouffa/emacs-ecryptfs")
  :when (and (or (featurep 'os/linux) (featurep 'os/bsd)) (executable-find "ecryptfs-verify"))
  :bind (:map minemacs-open-thing-map ("e" . ecryptfs-toggle-mount-private)))


(provide 'me-tools)

;;; me-tools.el ends here
