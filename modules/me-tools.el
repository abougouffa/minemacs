;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
(use-package ssh-deploy
  :straight t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :init
  (+map! "od" '(ssh-deploy-hydra/body :wk "ssh-deploy"))
  :config
  (ssh-deploy-hydra "C-c C-z"))

(use-package ack
  :straight t)

(use-package fzf
  :straight t
  :commands fzf-project
  :init
  (+map!
    "/"   #'fzf-project
    "sz" '(nil :wk "fzf")
    "szz" #'fzf
    "szg" #'fzf-grep
    "szG" #'fzf-grep-dwim
    "szf" #'fzf-find-file
    "szF" #'fzf-find-file-in-dir)
  :config
  (defun fzf-project (&optional with-preview)
    "Starts an fzf session at the root of the current project."
    (interactive "P")
    (let ((fzf/args (if with-preview (concat fzf/args " " fzf/args-for-preview) fzf/args))
          (fzf--target-validator (fzf--use-validator (function fzf--validate-filename))))
      (fzf--start (or (ignore-errors (project-root (project-current))) default-directory) #'fzf--action-find-file)))

  ;; fzf.el relays on `projectile-project-root' to guess the project root
  (unless (fboundp 'projectile-project-root)
    (defalias 'projectile-project-root (lambda () (ignore-errors (project-root (project-current)))))))

(use-package tldr
  :straight t
  :hook (minemacs-build-functions . tldr-update-docs)
  :hook (tldr-mode . visual-line-mode)
  :init
  (+map! "ht" #'tldr)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))

(use-package vterm
  :straight t
  :when (and (not os/win) (+emacs-features-p 'modules))
  :hook (minemacs-build-functions . vterm-module-compile)
  :bind (:map vterm-mode-map ("<return>" . vterm-send-return))
  :init
  (+map!
    "ot" '(nil :wk "vterm")
    "otv" (+def-dedicated-tab! vterm :exit-hook vterm-exit-functions))
  ;; Hide vterm install window
  (add-to-list 'display-buffer-alist
               `(" \\*Install vterm\\*"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-tramp-shells '(("docker" "/bin/bash")))
  :config
  (+imap! :keymaps 'vterm-mode-map
    "C-l" #'vterm-send-right
    "C-h" #'vterm-send-left
    "C-k" #'vterm-send-up
    "C-j" #'vterm-send-down))

(use-package multi-vterm
  :straight t
  :when (and (not os/win) (+emacs-features-p 'modules))
  :bind (("<remap> <project-shell>" . multi-vterm-project))
  :init
  (+map!
    "otT" #'multi-vterm
    "ott" #'multi-vterm-dedicated-toggle
    "otp" #'multi-vterm-project)
  ;; Show at bottom
  (add-to-list 'display-buffer-alist
               `("\\*vterminal - .*\\*" ;; multi-vterm-project / dedicated
                 (display-buffer-reuse-window display-buffer-in-direction)
                 (direction . bottom)
                 (dedicated . t)
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :config
  ;; If a dedicated terminal is run on a remote machine, it seems that
  ;; `multi-vterm' don't get the working directory right, lets fix it!
  (advice-add
   'multi-vterm-dedicated-open :after
   (defun +multi-vterm--remote-change-working-directory:after-a (&rest _)
     (if-let ((dir (file-remote-p default-directory 'localname)))
         (vterm-send-string (format "cd %S\n" dir)))))
  (+nvmap!
    :keymaps 'vterm-mode-map
    ",c" #'multi-vterm
    ",n" #'multi-vterm-next
    ",p" #'multi-vterm-prev
    "<return>" #'evil-insert-resume))

(use-package docker
  :straight t
  :init
  (+map! "ok" #'docker))

(use-package docker-compose-mode
  :straight t)

(use-package dockerfile-mode
  :straight t
  :unless (+emacs-features-p 'tree-sitter))

(use-package systemd
  :straight (:build (:not compile))
  :hook (systemd-mode . +systemd-mode-capf-h)
  :config
  (defun +systemd-mode-capf-h ()
    (add-hook 'completion-at-point-functions (cape-company-to-capf 'systemd-company-backend) -100)))

(use-package pkgbuild-mode
  :straight t
  :config
  (+map-local! :keymaps 'pkgbuild-mode-map
    "b" #'pkgbuild-makepkg
    "a" #'pkgbuild-tar
    "r" #'pkgbuild-increase-release-tag
    "u" #'pkgbuild-browse-url
    "m" #'pkgbuild-update-sums-line
    "s" #'pkgbuild-update-srcinfo
    "e" #'pkgbuild-etags))

(use-package journalctl-mode
  :straight t
  :config
  (+map-local! :keymaps 'journalctl-mode-map
    "J" #'journalctl-next-chunk
    "K" #'journalctl-previous-chunk))

(use-package logview
  :straight t
  :custom
  (logview-views-file (concat minemacs-local-dir "logview-views.el"))
  (logview-cache-filename (concat minemacs-cache-dir "logview-cache.el")))

(use-package with-editor
  :straight t
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . +with-editor-export-all)
  :init
  ;; `julia-repl' seems to start on `term-mode', so let's check for it before exporting the editor
  (defvar +with-editor-ignore-matching-buffers '("\\*julia\\*"))
  (defun +with-editor-export-all ()
    (unless (seq-some (+apply-partially-right #'string-match-p (buffer-name)) +with-editor-ignore-matching-buffers)
      (with-editor-export-editor)
      (with-editor-export-hg-editor)
      (with-editor-export-git-editor)))
  :bind (("<remap> <async-shell-command>" . with-editor-async-shell-command)
         ("<remap> <shell-command>" . with-editor-shell-command)))

(use-package app-launcher
  :straight (:host github :repo "SebastienWae/app-launcher")
  :when (or os/linux os/bsd)
  :init
  (+map! "oo" #'app-launcher-run-app))

(use-package nix-mode
  :straight t)

(use-package nix-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter)
  :mode "\\.nix\\'"
  :config
  ;; Register Eglot servers on the `nix-ts-mode' in addition to the already configured `nix-mode'
  (with-eval-after-load 'eglot
    (when-let ((server (assoc 'nix-mode eglot-server-programs)))
      (setcar server '(nix-mode nix-ts-mode)))))

(use-package envrc
  :straight t
  :hook (minemacs-first-file . envrc-global-mode)
  :when (and (not os/win) (executable-find "direnv"))
  :custom
  (envrc-debug minemacs-debug-p)
  :config
  ;; Ensure loading envrc for babel source blocks
  (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment))

(use-package osm
  :straight t)


(provide 'me-tools)

;;; me-tools.el ends here
