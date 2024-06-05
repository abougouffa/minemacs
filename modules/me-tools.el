;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
(use-package ssh-deploy
  :straight t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-hydra "C-c C-z"))

(use-package tmux
  :straight (:host github :repo "abougouffa/tmux.el")
  :unless os/win)

(use-package rg
  :straight t)

(use-package fzf
  :straight t
  :commands fzf-project
  :config
  (defalias 'fzf-project 'fzf-projectile)
  ;; fzf.el relays on `projectile-project-root' to guess the project root
  (unless (fboundp 'projectile-project-root)
    (provide 'projectile) ; provide `projectile' because `fzf-projectile' will try to require it
    (defalias 'projectile-project-root (lambda () (ignore-errors (project-root (project-current)))))))

(use-package tldr
  :straight t
  :hook (minemacs-build-functions . tldr-update-docs)
  :hook (tldr-mode . visual-line-mode)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))

(use-package vterm
  :straight t
  :when (and (not os/win) (+emacs-features-p 'modules))
  :hook (minemacs-build-functions . vterm-module-compile)
  :hook (vterm-mode . compilation-shell-minor-mode)
  :bind (:map vterm-mode-map ("<return>" . vterm-send-return))
  :init
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
  :bind (("<remap> <project-shell>" . multi-vterm-project))
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :config
  ;; If a dedicated terminal is run on a remote machine, it seems that
  ;; `multi-vterm' don't get the working directory right, lets fix it!
  (advice-add
   'multi-vterm-dedicated-open :after
   (satch-defun +multi-vterm--remote-change-working-directory:after-a (&rest _)
     (when-let ((dir (file-remote-p default-directory 'localname)))
       (vterm-send-string (format "cd %S\n" dir))))))

(use-package docker
  :straight t)

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
  :straight t)

(use-package journalctl-mode
  :straight t)

(use-package logview
  :straight t
  :custom
  (logview-views-file (concat minemacs-local-dir "logview-views.el"))
  (logview-cache-filename (concat minemacs-cache-dir "logview-cache.el"))
  (logview-additional-timestamp-formats '(("RDK" (java-pattern . "yyMMdd-HH:mm:ss.SSSSSS"))))
  (logview-additional-submodes '(("RDK Ccsp logs"
                                  (format . "TIMESTAMP [mod=NAME, lvl=LEVEL] [tid=THREAD]")
                                  (levels . "RDK"))))
  (logview-additional-level-mappings '(("RDK"
                                        (error "ERROR")
                                        (warning "WARN")
                                        (information "INFO")
                                        (debug "DEBUG")
                                        (trace "NOTICE")))))

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
  :when (or os/linux os/bsd))

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

(use-package pet
  :straight t
  :when (and (or (executable-find "dasel") (executable-find "yq"))
             (or (+emacs-features-p 'sqlite3) (executable-find "sqlite3")))
  :init
  (add-hook (if (< emacs-major-version 29) 'python-mode-hook 'python-base-mode-hook) #'pet-mode))

(use-package verb
  :straight t
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package restclient
  :straight (:host github :repo "abougouffa/restclient.el" :files ("*.el"))
  :hook (restclient-mode . display-line-numbers-mode)
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (require 'restclient-jq)
  (+setq-hook! restclient-mode
    imenu-generic-expression '((nil "^[A-Z]+\s+.+" 0)))

  ;; From Doom Emacs (in case `gnutls-verify-error' policy is set to something)
  (advice-add
   #'restclient-http-do :around
   (satch-defun +restclient--permit-self-signed-ssl:around-a (orig-fn &rest args)
     "Forces underlying SSL verification to prompt for self-signed or invalid
certs, rather than reject them silently."
     (require 'gnutls)
     (let (gnutls-verify-error) (apply orig-fn args)))))

(use-package restclient-test
  :straight t)

(use-package ob-restclient
  :straight t
  :after org
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((restclient . t))))

(use-package impostman
  :straight t)

(use-package hurl-mode
  :straight (:host github :repo "JasZhe/hurl-mode"))


(provide 'me-tools)

;;; me-tools.el ends here
