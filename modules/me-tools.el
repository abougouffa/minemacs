;; -*- lexical-binding: t; -*-


(use-package rg
  :straight t
  :general
  (+map "/" '(rg :wk "ripgrep")))

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
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000))

(use-package docker
  :straight t
  :general
  (+map "ok" #'docker))

(use-package dockerfile-mode
  :straight t
  :defer t)

(use-package esup
  :straight t
  :commands esup)

(provide 'me-tools)
