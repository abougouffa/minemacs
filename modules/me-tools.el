;; -*- lexical-binding: t; -*-


(use-package rg
  :straight t
  :general
  (+map :infix "s"
    "R" '(rg :wk "ripgrep")
    "r" #'rg-dwim))

(use-package affe
  :straight t
  :after consult orderless
  :general
  (+map
    "sg" #'affe-grep
    "sf" #'affe-find)
  :config
  ;; Use orderless to compile regexps
  (defun +affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))

  (setq affe-regexp-compiler #'+affe-orderless-regexp-compiler)

  ;; Manual preview keys
  (consult-customize affe-grep :preview-key (kbd "M-p"))
  (consult-customize affe-find :preview-key (kbd "M-p")))

;; Should be configured in per-project basis, good documentation at:
;; https://github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
(use-package ssh-deploy
  :straight t
  :defer t)

(use-package tldr
  :straight t
  :general
  (+map "ht" #'tldr)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))

(use-package vterm
  :straight t
  :general
  (+map "ot" #'vterm)
  :init
  ;; Hide vterm install window
  (add-to-list
   'display-buffer-alist
   `(" *Install vterm*"
     (display-buffer-no-window)
     (allow-no-window . t)))
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000))

(use-package docker
  :straight t
  :general
  (+map "ok" #'docker))

(unless (+emacs-features-p 'tree-sitter)
  ;; Emacs 29 comes with `dockerfile-ts-mode'
  (use-package dockerfile-mode
    :straight t
    :defer t))

(use-package esup
  :straight t
  :commands esup)

(use-package systemd
  :straight t
  :defer t)

(use-package pkgbuild-mode
  :straight t
  :defer t
  :config
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
  :defer t
  :config
  (+map-local :keymaps 'journalctl-mode-map
    "J" #'journalctl-next-chunk
    "K" #'journalctl-previous-chunk))

(use-package tramp
  :straight (:type built-in)
  :defer t
  :init
  ;; This is faster than the default "scp"
  (unless os/win
    (setq tramp-default-method "ssh")))

(provide 'me-tools)
