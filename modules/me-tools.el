;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
(use-package ssh-deploy
  :elpaca t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :init
  (+map! "od" '(ssh-deploy-hydra/body :wk "ssh-deploy"))
  :config
  (ssh-deploy-hydra "C-c C-z"))

(use-package tldr
  :elpaca t
  :hook (minemacs-build-functions . tldr-update-docs)
  :hook (tldr-mode . visual-line-mode)
  :init
  (+map! "ht" #'tldr)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))

(use-package vterm
  ;; Using the default recipe will cause `vterm' to ask about compilation, which
  ;; blocks elpaca. This hack is taken from github.com/progfolio/.emacs.d#vterm
  :elpaca (vterm :post-build
                 (progn
                   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;; print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S" (buffer-substring (line-beginning-position) (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists))))
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
  (vterm-tramp-shells '(("docker" "/bin/bash"))))

(use-package multi-vterm
  :elpaca t
  :when (and (not os/win) (+emacs-features-p 'modules))
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
  (+nvmap!
    :keymaps 'vterm-mode-map
    ",c" #'multi-vterm
    ",n" #'multi-vterm-next
    ",p" #'multi-vterm-prev
    "<return>" #'evil-insert-resume))

(use-package docker
  :elpaca t
  :init
  (+map! "ok" #'docker))

(use-package docker-compose-mode
  :elpaca t)

(use-package dockerfile-mode
  :elpaca t
  :unless (+emacs-features-p 'tree-sitter))

(use-package systemd
  :elpaca (:build (:not elpaca--byte-compile))
  :hook (systemd-mode . +systemd-mode-capf-h)
  :config
  (defun +systemd-mode-capf-h ()
    (add-hook 'completion-at-point-functions (cape-company-to-capf 'systemd-company-backend) -100)))

(use-package pkgbuild-mode
  :elpaca t
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
  :elpaca t
  :config
  (+map-local! :keymaps 'journalctl-mode-map
    "J" #'journalctl-next-chunk
    "K" #'journalctl-previous-chunk))

(use-package logview
  :elpaca t
  :custom
  (logview-views-file (concat minemacs-local-dir "logview-views.el"))
  (logview-cache-filename (concat minemacs-cache-dir "logview-cache.el")))

(use-package bitwarden
  :elpaca (:host github :repo "seanfarley/emacs-bitwarden")
  :when (executable-find "bw")
  :custom
  (bitwarden-automatic-unlock
   (lambda ()
     (require 'auth-source)
     (if-let* ((matches (auth-source-search :host "bitwarden.com" :max 1))
               (entry (nth 0 matches))
               (email (plist-get entry :user))
               (pass (plist-get entry :secret)))
         (progn (setq bitwarden-user email)
                (if (functionp pass) (funcall pass) pass))
       ""))))

(use-package with-editor
  :elpaca t
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
  :elpaca (:host github :repo "SebastienWae/app-launcher")
  :when (or os/linux os/bsd)
  :init
  (+map! "oo" #'app-launcher-run-app))

(use-package nix-mode
  :elpaca t)

(use-package nix-ts-mode
  :elpaca t
  :when (+emacs-features-p 'tree-sitter)
  :mode "\\.nix\\'"
  :config
  ;; Register Eglot servers on the `nix-ts-mode' in addition to the already configured `nix-mode'
  (with-eval-after-load 'eglot
    (when-let ((server (assoc 'nix-mode eglot-server-programs)))
      (setcar server '(nix-mode nix-ts-mode)))))

(use-package nix-update
  :elpaca t
  :when (executable-find "nix"))

(use-package envrc
  :elpaca t
  :hook (minemacs-first-file . envrc-global-mode)
  :when (executable-find "direnv")
  :custom
  (envrc-debug minemacs-debug-p))

(use-package guix
  :elpaca t
  :when (executable-find "guix")
  :init
  (+map! "og" #'guix))


(provide 'me-tools)

;;; me-tools.el ends here
