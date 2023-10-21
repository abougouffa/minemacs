;; -*- lexical-binding: t; -*-

;;; Commentary:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples

;;; Code:

(use-package affe
  :straight t
  :init
  (+map! :infix "s"
    "G" #'affe-grep
    "F" #'affe-find)
  :config
  (with-eval-after-load 'orderless
    (defun +affe-orderless-regexp-compiler (input &rest _)
      (setq input (orderless-pattern-compiler input))
      (cons input (apply-partially #'orderless--highlight input)))
    (setq affe-regexp-compiler #'+affe-orderless-regexp-compiler)))

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
  :hook (minemacs-build-functions . vterm-module-compile)
  :init
  (+map!
    "ot" '(nil :wk "vterm")
    "otT" (+def-dedicated-tab! vterm :exit-hook vterm-exit-functions))
  ;; Hide vterm install window
  (add-to-list
   'display-buffer-alist
   `(" \\*Install vterm\\*"
     (display-buffer-no-window)
     (allow-no-window . t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-tramp-shells '(("docker" "/bin/bash")))
  :config
  (keymap-set vterm-mode-map "<return>" 'vterm-send-return))

(use-package multi-vterm
  :straight t
  :init
  (+map!
    "ott" #'multi-vterm
    "otd" #'multi-vterm-dedicated-toggle
    "otp" #'multi-vterm-project)
  ;; Show at buttom
  (add-to-list
   'display-buffer-alist
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

(when (+emacs-features-p 'tree-sitter)
  (push '(tree-sitter dockerfile-mode) minemacs-disabled-packages))

(use-package dockerfile-mode
  :straight t)

(use-package systemd
  :straight (systemd :build (:not compile)))

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

(defconst +bitwarden-available-p (executable-find "bw"))

(use-package bitwarden
  :straight (:host github :repo "seanfarley/emacs-bitwarden")
  :when +bitwarden-available-p
  :custom
  (bitwarden-automatic-unlock
   (lambda ()
     (require 'auth-source)
     (if-let* ((matches (auth-source-search :host "bitwarden.com" :max 1))
               (entry (nth 0 matches))
               (email (plist-get entry :user))
               (pass (plist-get entry :secret)))
         (progn
           (setq bitwarden-user email)
           (if (functionp pass) (funcall pass) pass))
       ""))))

(use-package chezmoi
  :straight t
  :commands
  chezmoi-find chezmoi-write chezmoi-diff chezmoi-ediff
  chezmoi-open-other chezmoi-sync-files chezmoi-magit-status
  :init
  (+map! :infix "o"
    "c" '(nil :wk "chezmoi")
    "cf" #'chezmoi-find
    "cw" #'chezmoi-write
    "cd" #'chezmoi-diff
    "ce" #'chezmoi-ediff
    "co" #'chezmoi-open-other
    "cs" #'chezmoi-sync-files
    "cg" #'chezmoi-magit-status))


(provide 'me-tools)

;;; me-tools.el ends here
