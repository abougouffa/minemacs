;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2025-07-19

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


;; Launch system applications from Emacs
(use-package app-launcher
  :straight (:host github :repo "SebastienWae/app-launcher")
  :when (or (featurep 'os/linux) (featurep 'os/bsd))
  :bind (:map minemacs-open-thing-map ("a" . app-launcher-run-app)))


;; Manipulate "tmux" from Emacs
(use-package emamux
  :straight t)


;; System-wide popup Emacs windows for quick edits
(use-package emacs-everywhere
  :straight t)


;; Browse "tldr" pages from Emacs
(use-package tldr
  :straight t
  :hook
  (minemacs-build-functions . tldr-update-docs)
  (tldr-mode . visual-line-mode)
  :custom
  (tldr-enabled-categories '("common" "linux" "netbsd" "openbsd" "freebsd" "osx" "windows")))


;; Fully-fledged terminal emulator inside Emacs based on "libvterm"
(use-package vterm
  :straight t
  :when (and (not (featurep 'os/win)) (featurep 'feat/modules))
  :hook
  (minemacs-build-functions . vterm-module-compile)
  (vterm-mode . compilation-shell-minor-mode)
  (vterm-mode . minemacs-reduce-font-size)
  :bind (:map vterm-mode-map ([return] . vterm-send-return))
  :init
  (+def-dedicated-tab! vterm :exit-hook vterm-exit-functions)
  :custom
  (vterm-module-cmake-args "-DUSE_SYSTEM_LIBVTERM=Off")
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  :config
  (add-to-list 'display-buffer-alist `("\\*Install vterm\\*" (display-buffer-no-window) (allow-no-window . t))))


;; Managing multiple vterm buffers in Emacs
(use-package multi-vterm
  :straight t
  :when (and (not (featurep 'os/win)) (featurep 'feat/modules))
  :bind (([remap project-shell] . multi-vterm-project)
         ([f1] . +multi-vterm-toggle-dwim)
         :map vterm-mode-map ([f1] . +multi-vterm-toggle-dwim))
  :custom
  (multi-vterm-dedicated-window-height-percent 20)
  :config
  ;; If a dedicated terminal is run on a remote machine, it seems that
  ;; `multi-vterm' don't get the working directory right, lets fix it!
  (advice-add
   'multi-vterm-dedicated-open :after
   (satch-defun +multi-vterm--remote-change-working-directory:after-a (&rest _)
     (when-let* ((dir (file-remote-p default-directory 'localname)))
       (vterm-send-string (format "cd " (shell-quote-argument dir)))
       (vterm-send-return))))

  (defun +multi-vterm-toggle-dwim ()
    "Toggle the vterm window.
When in a project, toggle a `multi-vterm-project' terminal. When outside
a project, call `multi-vterm-dedicated-toggle'."
    (interactive)
    (if-let* ((buf-name (and (multi-vterm-project-root) (multi-vterm-project-get-buffer-name))))
        (if-let* ((buf (get-buffer buf-name))
                  ((buffer-live-p buf)))
            (if-let* ((win (get-buffer-window buf))) ; The project's vterm already exists, toggle it's window
                (delete-window win)
              (pop-to-buffer buf))
          (multi-vterm-project))
      (multi-vterm-dedicated-toggle))))


;; Manage docker from Emacs
(use-package docker
  :straight t
  :bind (:map minemacs-open-thing-map ("d" . docker)))


;; Rudimentary devcontainer support for Emacs
(use-package devcontainer
  :straight (:host github :repo "johannes-mueller/devcontainer.el"))


;; Major mode for Apptainer definition files
(use-package apptainer-mode
  :straight (:host github :repo "jrgant/apptainer-mode")
  :config
  (setq apptainer-boxed-headers t
        apptainer-boxed-sections t))


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
  :bind (:map minemacs-open-thing-map ("v" . envrc-command-map))
  :custom
  (envrc-debug minemacs-debug-p)
  (envrc-remote t)
  (envrc-supported-tramp-methods '("ssh" "docker"))
  :config
  ;; Ensure loading envrc for babel source blocks
  (with-eval-after-load 'ob
    (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)))


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
