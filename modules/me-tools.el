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
  (+map
    "ot" '(+popwin:vterm :wk "vTerm popup")
    "oT" '(vterm :wk "vTerm"))
  :preface
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :custom
  (vterm-max-scrollback 5000)
  :config
  (with-eval-after-load 'popwin
    (defun +popwin:vterm ()
      (interactive)
      (popwin:display-buffer-1
       (or (get-buffer "*vterm*")
           (save-window-excursion
             (call-interactively 'vterm)))
       :default-config-keywords '(:position :bottom :height 12)))))


(use-package docker
  :straight t
  :general
  (+map "ok" #'docker))


(use-package dockerfile-mode
  :straight t
  :defer t)


(use-package eshell
  :defer t
  :general
  (+map "oE" '(eshell :wk "Eshell"))
  :config
  (with-eval-after-load 'popwin
    (+map "oe" '(+popwin:eshell :wk "Eshell popup"))
    (defun +popwin:eshell ()
      (interactive)
      (popwin:display-buffer-1
       (or (get-buffer "*eshell*")
           (save-window-excursion
             (call-interactively 'eshell)))
       :default-config-keywords '(:position :bottom :height 12)))))


(provide 'me-tools)
