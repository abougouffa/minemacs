;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples

;;; Code:

(use-package ssh-deploy
  :straight t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :init
  (+map! "od" '(ssh-deploy-hydra/body :wk "ssh-deploy"))
  :config
  (ssh-deploy-hydra "C-c C-z"))

(use-package tldr
  :straight t
  :hook (minemacs-build-functions . tldr-update-docs)
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
  (vterm-tramp-shells '(("docker" "/bin/bash"))))

(use-package multi-vterm
  :straight t
  :when (and (not os/win) (+emacs-features-p 'modules))
  :init
  (+map!
    "otT" #'multi-vterm
    "ott" #'multi-vterm-dedicated-toggle
    "otp" #'multi-vterm-project)
  ;; Show at buttom
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
  :straight t
  :init
  (+map! "ok" #'docker))

(use-package docker-compose-mode
  :straight t)

(use-package dockerfile-mode
  :straight t
  :unless (+emacs-features-p 'tree-sitter))

(use-package systemd
  :straight (systemd :build (:not compile))
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

(use-package bitwarden
  :straight (:host github :repo "seanfarley/emacs-bitwarden")
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
  :straight t
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . with-editor-export-editor)
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . with-editor-export-hg-editor)
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . with-editor-export-git-editor)
  :bind (("<remap> <async-shell-command>" . with-editor-async-shell-command)
         ("<remap> <shell-command>" . with-editor-shell-command)))

(use-package app-launcher
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :when (or os/linux os/bsd)
  :init
  (+map! "oo" #'app-launcher-run-app))


(provide 'me-tools)

;;; me-tools.el ends here
