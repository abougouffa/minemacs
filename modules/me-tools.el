;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; A deployment plugin via Tramp for Emacs
;; Should be configured in per-project basis, good documentation at:
;; https://github.com/cjohansson/emacs-ssh-deploy#deployment-configuration-examples
(use-package ssh-deploy
  :straight t
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :bind (("C-c C-z" . ssh-deploy-prefix-map))
  :custom
  (ssh-deploy-revision-folder (concat minemacs-cache-dir "ssh-deploy-revisions/")))


;; TRAMP integration for Incus containers
(use-package incus-tramp
  :straight t
  :after tramp
  :init
  (incus-tramp-add-method))


;; Launch system applications from Emacs
(use-package app-launcher
  :straight (:host github :repo "SebastienWae/app-launcher")
  :when (+emacs-options-p :any 'os/linux 'os/bsd)
  :bind (:map minemacs-open-thing-map ("a" . app-launcher-run-app)))


;; System-wide popup Emacs windows for quick edits
(use-package emacs-everywhere
  :straight t)


;; Browse "tldr" pages from Emacs
(use-package tldr
  :straight t
  :hook (minemacs-build-functions . tldr-update-docs)
  :hook (tldr-mode . visual-line-mode)
  :custom
  (tldr-enabled-categories '("common" "linux" "osx")))


;; Fully-fledged terminal emulator inside Emacs based on "libvterm"
(use-package vterm
  :straight t
  :when (and (not (+emacs-options-p 'os/win)) (+emacs-options-p 'modules))
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


;; Managing multiple vterm buffers in Emacs
(use-package multi-vterm
  :straight t
  :when (and (not (+emacs-options-p 'os/win)) (+emacs-options-p 'modules))
  :bind (([remap project-shell] . multi-vterm-project)
         ([f1] . +multi-vterm-dedicated-toggle-dwim)
         :map vterm-mode-map ([f1] . +multi-vterm-dedicated-toggle-dwim))
  :custom
  (multi-vterm-dedicated-window-height-percent 30)
  :config
  ;; If a dedicated terminal is run on a remote machine, it seems that
  ;; `multi-vterm' don't get the working directory right, lets fix it!
  (advice-add
   'multi-vterm-dedicated-open :after
   (satch-defun +multi-vterm--remote-change-working-directory:after-a (&rest _)
     (when-let* ((dir (file-remote-p default-directory 'localname)))
       (vterm-send-string (format "cd %S\n" dir)))))

  (defun +multi-vterm-dedicated-toggle-dwim ()
    "Toggle the vterm window.
When in a project, toggle a `multi-vterm-project' terminal. When outside
a project, call `multi-vterm-dedicated-toggle'."
    (interactive)
    (if-let* ((buf-name (and (multi-vterm-project-root) (multi-vterm-project-get-buffer-name)))
              (display-buffer-alist (cons `(,(regexp-quote buf-name)
                                            (display-buffer-reuse-window display-buffer-at-bottom)
                                            (dedicated . t) ;; Close when finished
                                            (window-height . 0.3))
                                          display-buffer-alist)))
        (if-let* ((buf (get-buffer buf-name))
                  ((buffer-live-p buf)))
            (if-let ((win (get-buffer-window buf))) ; The project's vterm already exists, toggle it's window
                (delete-window win)
              (pop-to-buffer buf))
          (multi-vterm-project))
      (multi-vterm-dedicated-toggle))))


;; Manage docker from Emacs
(use-package docker
  :straight t
  :bind (:map minemacs-open-thing-map ("d" . docker)))


;; Major mode for editing systemd units
(use-package systemd
  :straight (:host github :repo "holomorph/systemd-mode" :fork (:repo "abougouffa/systemd-mode")))


;; Major mode to view journalctl's output in Emacs
(use-package journalctl-mode
  :straight t
  :commands (journalctl-mode))


;; Emacs mode for viewing log files
(use-package logview
  :straight t
  :custom
  (logview-additional-timestamp-formats '(("RDK-CCSP" (java-pattern . "yyMMdd-HH:mm:ss.SSSSSS"))))
  (logview-additional-submodes '(("RDK-CCSP" (format . "TIMESTAMP [mod=NAME, lvl=LEVEL] [tid=THREAD]") (levels . "RDK-CCSP"))))
  (logview-additional-level-mappings '(("RDK-CCSP" (error "ERROR") (warning "WARN") (information "INFO") (debug "DEBUG") (trace "NOTICE")))))


;; Use the Emacsclient as the "$EDITOR" of child processes
(use-package with-editor
  :straight t
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . +with-editor-export-all)
  :init
  ;; `julia-repl' seems to start on `term-mode', so let's check for it before exporting the editor
  (defvar +with-editor-ignore-matching-buffers '("\\*julia\\*"))
  (defun +with-editor-export-all ()
    (unless (seq-some (+apply-partially-right #'string-match-p (buffer-name)) +with-editor-ignore-matching-buffers)
      (+shutup! (with-editor-export-editor)) ; Export EDITOR
      (+shutup! (with-editor-export-hg-editor)) ; Export HG_EDITOR
      (+shutup! (with-editor-export-git-editor)) ; Export GIT_EDITOR
      (+shutup! (with-editor-export-editor "JJ_EDITOR")))) ; Export JJ_EDITOR
  :bind (("<remap> <async-shell-command>" . with-editor-async-shell-command)
         ("<remap> <shell-command>" . with-editor-shell-command))
  :config
  (add-to-list 'with-editor-envvars "JJ_EDITOR")) ; Add support for Jujutsu (`jj')


;; Buffer-local "direnv" integration for Emacs
(use-package envrc
  :straight t
  :hook (minemacs-first-file . envrc-global-mode)
  :when (and (not (+emacs-options-p 'os/win)) (executable-find "direnv"))
  :custom
  (envrc-debug minemacs-debug-p)
  :config
  ;; Ensure loading envrc for babel source blocks
  (with-eval-after-load 'ob
    (advice-add #'org-babel-execute-src-block :around #'envrc-propagate-environment)))


;; Python Executable Tracker
(use-package pet
  :straight t
  :when (and (or (executable-find "dasel") (executable-find "yq"))
             (or (+emacs-options-p 'sqlite3) (executable-find "sqlite3")))
  :hook (pet-mode . +pet-quickrun-setup)
  :init
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  :config
  (defun +pet-quickrun-setup ()
    (with-eval-after-load 'quickrun
      (let* ((cmd-alist (copy-alist (alist-get "python" quickrun--language-alist nil nil #'equal))))
        (setcar (assq :command cmd-alist) (pet-executable-find "python"))
        (setq-local quickrun-option-cmd-alist cmd-alist)))))


;; Adds the "node_modules/.bin" directory to the buffer "exec_path"
(use-package add-node-modules-path
  :straight t
  :hook (js-base-mode . add-node-modules-path)
  :config
  (when (executable-find "pnpm")
    (setopt add-node-modules-path-command '("pnpm bin" "pnpm bin -w"))))


;; Organize and send HTTP requests from Emacs' Org mode files
(use-package verb
  :straight t
  :config
  (keymap-set org-mode-map "C-c C-r" `("verb" . ,verb-command-map)))


;; Import of Postman collections in Emacs (for `verb' and `restclient')
(use-package impostman
  :straight t)


;; Mount/umount eCryptfs private directory from Emacs
(use-package ecryptfs
  :straight (:host github :repo "abougouffa/emacs-ecryptfs")
  :when (and (+emacs-options-p :any 'os/linux 'os/bsd) (executable-find "ecryptfs-verify"))
  :bind (:map minemacs-open-thing-map ("e" . ecryptfs-toggle-mount-private)))


(provide 'me-tools)

;;; me-tools.el ends here
