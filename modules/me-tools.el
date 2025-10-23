;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-10-23

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
  :bind ([f1] . +eat-toggle-dwim)
  :config
  (advice-add 'eat--sentinel :around #'+kill-buffer-after-sentinel-exit)
  (defun +eat-toggle-dwim ()
    "Toggle the EAT window.
When in a project, toggle `eat-project', else, toggle `eat'."
    (interactive)
    (let* ((buf-name (if (project-current) (project-prefixed-buffer-name "eat") eat-buffer-name))
           (eat-func (if (project-current) #'eat-project #'eat)))
      (if-let* ((buf (get-buffer buf-name))
                ((buffer-live-p buf)))
          (if-let* ((win (get-buffer-window buf)))
              (delete-window win)
            (pop-to-buffer buf))
        (call-interactively eat-func)))))


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


;; Python Executable Tracker
(use-package pet
  :straight t
  :when (and (or (executable-find "dasel") (executable-find "yq"))
             (or (featurep 'feat/sqlite3) (executable-find "sqlite3")))
  :init
  ;; BUG: When accessing files via ADB, `pet-mode' fails at some stage because
  ;; `tramp' isn't able to give a relavant information in
  ;; `tramp-handle-file-directory-p'. After tracing this down, it seems like
  ;; `file-attributes' doesn't support my "adb" for now.
  (defun +pet-mode-maybe ()
    (when-let* ((path (or (buffer-file-name (or (buffer-base-buffer) (current-buffer))) default-directory))
                ((not (file-remote-p path))))
      (pet-mode 1)))

  ;; TODO: Try to find a better way of applying `pet-mode', currently, it slows
  ;; down opening Python buffers (or reverting them)
  (add-hook 'python-base-mode-hook '+pet-mode-maybe -10))


;; Mount/umount eCryptfs private directory from Emacs
(use-package ecryptfs
  :straight (:host github :repo "abougouffa/emacs-ecryptfs")
  :when (and (or (featurep 'os/linux) (featurep 'os/bsd)) (executable-find "ecryptfs-verify"))
  :bind (:map minemacs-open-thing-map ("e" . ecryptfs-toggle-mount-private)))


(provide 'me-tools)

;;; me-tools.el ends here
