;; -*- lexical-binding: t; -*-


;; Should be configured in per-project basis, good documentation at:
;; https://github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
(use-package ssh-deploy
  :straight t)

(use-package tldr
  :straight t
  :general
  (+map "ht" #'tldr)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))

(use-package vterm
  :straight t
  :general
  (+map
    "ot" '(nil :wk "vterm")
    "otT" #'vterm)
  :init
  ;; Hide vterm install window
  (add-to-list
   'display-buffer-alist
   `(" \\*Install vterm\\*"
     (display-buffer-no-window)
     (allow-no-window . t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  :config
  (define-key vterm-mode-map [return] #'vterm-send-return))

(use-package multi-vterm
  :straight t
  :general
  (+map
    "ott" #'multi-vterm
    "otd" #'multi-vterm-dedicated-toggle
    "otp" #'multi-vterm-project)
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :init
  ;; Show at buttom
  (add-to-list
   'display-buffer-alist
   `("\\*vterminal - .*\\*" ;; multi-vterm-project
     (display-buffer-reuse-window display-buffer-in-direction)
     (direction . bottom)
     (dedicated . t)
     (reusable-frames . visible)
     (window-height . 0.3)))
  :config
  (+map-key
    :keymaps 'vterm-mode-map
    ",c" #'multi-vterm
    ",n" #'multi-vterm-next
    ",p" #'multi-vterm-prev
    "<return>" #'evil-insert-resume))

(use-package docker
  :straight t
  :general
  (+map "ok" #'docker))

(unless (+emacs-features-p 'tree-sitter)
  ;; Emacs 29 comes with `dockerfile-ts-mode'
  (use-package dockerfile-mode
    :straight t))

(use-package systemd
  :straight t)

(use-package pkgbuild-mode
  :straight t
  :general
  (+map-local :keymaps 'pkgbuild-mode-map
    "b" #'pkgbuild-makepkg
    "a" #'pkgbuild-tar
    "r" #'pkgbuild-increase-release-tag
    "u" #'pkgbuild-browse-url
    "m" #'pkgbuild-update-sums-line
    "s" #'pkgbuild-update-srcinfo
    "e" #'pkgbuild-etags))

(use-package journalctl-mode
  :straight t
  :general
  (+map-local :keymaps 'journalctl-mode-map
    "J" #'journalctl-next-chunk
    "K" #'journalctl-previous-chunk))

(use-package tramp
  :straight (:type built-in)
  :init
  ;; This is faster than the default "scp"
  (unless os/win
    (setq tramp-default-method "ssh")))

(use-package bitwarden
  :straight (:host github :repo "seanfarley/emacs-bitwarden")
  :preface
  (defconst +bitwarden-available-p (executable-find "bw"))
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


(provide 'me-tools)
