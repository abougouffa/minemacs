;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Should be configured in per-project basis, good documentation at:
;; github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
(use-package ssh-deploy
  :straight t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :custom
  (ssh-deploy-revision-folder (concat minemacs-cache-dir "ssh-deploy-revisions/"))
  :config
  (ssh-deploy-hydra "C-c C-z"))

(use-package app-launcher
  :straight (:host github :repo "SebastienWae/app-launcher")
  :when (or os/linux os/bsd)
  :bind (:map minemacs-open-thing-map ("a" . app-launcher-run-app)))

(use-package emamux
  :straight t)

(use-package emacs-everywhere
  :straight t)

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
  :bind (:map vterm-mode-map ([return] . vterm-send-return))
  :init
  ;; Hide vterm install window
  (add-to-list 'display-buffer-alist
               `(" \\*Install vterm\\*"
                 (display-buffer-no-window)
                 (allow-no-window . t)))
  (+def-dedicated-tab! vterm :exit-hook vterm-exit-functions)
  :custom
  (vterm-always-compile-module t)
  (vterm-max-scrollback 5000)
  (vterm-tramp-shells '(("docker" "/bin/bash"))))

(use-package multi-vterm
  :straight t
  :when (and (not os/win) (+emacs-features-p 'modules))
  :bind (([remap project-shell] . multi-vterm-project)
         ([f1] . multi-vterm-project))
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
  :straight t
  :bind (("C-c o d" . docker)))

(use-package systemd
  :straight (:host github :repo "holomorph/systemd-mode" :fork (:repo "abougouffa/systemd-mode")))

(use-package journalctl-mode
  :straight t
  :commands (journalctl-mode))

(use-package logview
  :straight t
  :custom
  (logview-additional-timestamp-formats '(("RDK-CCSP" (java-pattern . "yyMMdd-HH:mm:ss.SSSSSS"))))
  (logview-additional-submodes '(("RDK-CCSP" (format . "TIMESTAMP [mod=NAME, lvl=LEVEL] [tid=THREAD]") (levels . "RDK-CCSP"))))
  (logview-additional-level-mappings '(("RDK-CCSP" (error "ERROR") (warning "WARN") (information "INFO") (debug "DEBUG") (trace "NOTICE")))))

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

(use-package envrc
  :straight t
  :hook (minemacs-first-file . envrc-global-mode)
  :when (and (not os/win) (executable-find "direnv"))
  :custom
  (envrc-debug minemacs-debug-p)
  :config
  ;; Ensure loading envrc for babel source blocks
  (with-eval-after-load 'ob
    (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)))

(use-package pet
  :straight t
  :when (and (or (executable-find "dasel") (executable-find "yq"))
             (or (+emacs-features-p 'sqlite3) (executable-find "sqlite3")))
  :init
  (add-hook (if (< emacs-major-version 29) 'python-mode-hook 'python-base-mode-hook) #'pet-mode))

(use-package add-node-modules-path
  :straight t
  :hook ((js-mode js-ts-mode js2-mode) . add-node-modules-path)
  :config
  (when (executable-find "pnpm")
    (setopt add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))))

(use-package verb
  :straight t
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package impostman
  :straight t)

(use-package ecryptfs
  :straight (:host github :repo "abougouffa/emacs-ecryptfs")
  :when (and (or os/linux os/bsd) (executable-find "ecryptfs-verify"))
  :bind (("C-c o e" . ecryptfs-toggle-mount-private)))


(provide 'me-tools)

;;; me-tools.el ends here
