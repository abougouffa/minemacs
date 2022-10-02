;; -*- lexical-binding: t; -*-

(use-package rg
  :straight t
  :defer t
  :general
  (me-global-def "/" '(rg :which-key "ripgrep")))

(use-package vterm
  :straight t
  :when (featurep 'dynamic-modules)
  :defer t
  :general
  (me-global-def "ot" '(vterm :which-key "vTerm"))
  :commands vterm-mode
  :hook (vterm-mode . hide-mode-line-mode)
  :preface
  (when noninteractive
    (advice-add #'vterm-module-compile :override #'ignore)
    (provide 'vterm-module))
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000)
  (add-hook 'vterm-mode-hook
            (lambda ()
              (setq confirm-kill-processes nil
                    hscroll-margin 0))))
