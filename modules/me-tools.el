;;  me-tools.el -- System tools -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2026-07-09

;;; Commentary:

;;; Code:

;; A deployment plugin via Tramp for Emacs
(use-package ssh-deploy
  :straight (ssh-deploy :source gnu-elpa-mirror)
  ;; Should be configured in per-project basis, good documentation at:
  ;; https://github.com/emacs-straight/ssh-deploy#deployment-configuration-examples
  :hook
  (after-save . ssh-deploy-after-save)
  (find-file . ssh-deploy-find-file)
  :bind (("C-c C-z" . ssh-deploy-prefix-map))
  :custom
  (ssh-deploy-revision-folder (concat minemacs-cache-dir "ssh-deploy-revisions/")))


;; Emulate A Terminal, in a region, in a buffer and in Eshell
(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :autoload eat-make
  :init
  (+def-dedicated-tab! eat :exit-hook eat-exit-hook)
  (+super-project-define-commands 'eat-project)
  :config
  (advice-add 'eat--sentinel :around #'+kill-buffer-after-sentinel-exit)
  (defun +eat-toggle-dwim ()
    "Toggle the EAT window.
When in a project, toggle `eat-project', else, toggle `eat'."
    (interactive)
    (let* ((buf-name (if (project-current) (project-prefixed-buffer-name "eat") eat-buffer-name))
           (eat-func (if (project-current) #'eat-project #'eat)))
      (if-let* ((buf (get-buffer buf-name))
                ((buffer-live-p buf)))
          (if-let* ((win (get-buffer-window buf)))
              (delete-window win)
            (pop-to-buffer buf))
        (call-interactively eat-func)))))


;; Terminal emulator powered by libghostty
(use-package ghostel
  :straight t
  :bind ([f1] . +ghostel-toggle-dwim)
  :hook
  (minemacs-build-functions . ghostel-download-module)
  (eshell-load . ghostel-eshell-visual-command-mode)
  (minemacs-lazy . ghostel-compile-global-mode)
  (minemacs-lazy . ghostel-comint-global-mode)
  :init
  (+def-dedicated-tab! ghostel :exit-hook ghostel-exit-functions)
  (+super-project-define-commands 'ghostel-project)
  :config
  ;; TWEAK: Bind the F1 key to toggle in `ghostel' modes, otherwise, it will be bound to `ghostel--send-event'
  (dolist (map (list ghostel-mode-map ghostel-line-mode-map ghostel-readonly-mode-map ghostel-char-mode-map ghostel-semi-char-mode-map))
    (keymap-set map "<f1>" #'+ghostel-toggle-dwim))

  (defvar +ghostel-proj-terminals nil)
  (defun +ghostel-toggle-dwim (arg)
    "Toggle the Ghostel window, step to the current directory when ARG.
When in a project, toggle `ghostel-project', else, toggle `ghostel'."
    (interactive "P")
    (let* ((proj (project-current))
           (key (or (+project-safe-root proj) "default"))
           (ghostel-func (if proj #'ghostel-project #'ghostel))
           (buf (alist-get key +ghostel-proj-terminals nil nil #'equal))
           (created (and buf (buffer-live-p buf)))
           (running (and created (get-buffer-process buf)))
           (visible-win (and buf (get-buffer-window buf)))
           (target-dir (and arg (not visible-win) (shell-quote-argument (file-local-name (expand-file-name default-directory))))))
      (if visible-win
          (delete-window visible-win)
        (if running
            (pop-to-buffer buf)
          (setq buf (call-interactively ghostel-func))
          (+alist-set! key buf +ghostel-proj-terminals)))
      (when target-dir
        (with-current-buffer buf
          (ghostel-send-C-c)
          (ghostel-send-string (concat "cd " target-dir))
          (ghostel-send-key))))))

;; Launch system applications from Emacs
(use-package xdg-launcher
  :straight (:host github :repo "emacs-exwm/xdg-launcher")
  :when (or (featurep 'os/linux) (featurep 'os/bsd))
  :bind (:map minemacs-open-thing-map ("a" . xdg-launcher-run-app))
  :config
  (when (featurep 'eat)
    (setopt xdg-launcher-terminal-function #'eat-make)))


;; Manage docker from Emacs
(use-package docker
  :straight t
  :bind (:map minemacs-open-thing-map ("d" . docker)))


;; Rudimentary devcontainer support for Emacs
(use-package devcontainer
  :straight (:host github :repo "johannes-mueller/devcontainer.el")
  :hook (minemacs-after-startup . devcontainer-mode)
  :custom
  (devcontainer-term-function (cond ((featurep 'eat) 'eat)
                                    (t 'ansi-term)))
  :config
  ;; BUGFIX: When enabling `devcontainer-mode' globally, this can trigger errors for non-project files
  (advice-add 'devcontainer-config-files :around (lambda (fn) (ignore-errors (funcall fn)))))


;; Major mode to view journalctl's output in Emacs
(use-package journalctl-mode
  :straight t
  :commands (journalctl-mode))


;; Use the Emacsclient as the "$EDITOR" of child processes
(use-package with-editor
  :straight t
  :hook ((shell-mode eshell-mode term-exec vterm-mode) . +with-editor-export-editor-maybe)
  :init
  (+add-transient-advice 'shell-command :before (lambda (&rest _args) (shell-command-with-editor-mode 1)))
  ;; `julia-repl' seems to start on `term-mode', so let's check for it before exporting the editor
  (defvar +with-editor-ignore-matching-buffers '("\\*julia\\*"))
  (defun +with-editor-export-editor-maybe ()
    (unless (seq-some (+apply-partially-right #'string-match-p (buffer-name)) +with-editor-ignore-matching-buffers)
      (+shutup! (with-editor-export-editor)))))


;; Python Executable Tracker
(use-package pet
  :straight t
  :when (and (or (executable-find "dasel") (executable-find "yq"))
             (or (featurep 'feat/sqlite3) (executable-find "sqlite3")))
  :init
  ;; BUG: When accessing files via ADB, `pet-mode' fails at some stage because
  ;; `tramp' isn't able to give a relavant information in
  ;; `tramp-handle-file-directory-p'. After tracing this down, it seems like
  ;; `file-attributes' doesn't support my "adb" for now.
  (defun +pet-mode-maybe ()
    (when-let* ((path (or (buffer-file-name (or (buffer-base-buffer) (current-buffer))) default-directory))
                ((not (file-remote-p path))))
      (pet-mode 1)))

  ;; TODO: Try to find a better way of applying `pet-mode', currently, it slows
  ;; down opening Python buffers (or reverting them)
  (add-hook 'python-base-mode-hook '+pet-mode-maybe -10)
  :config
  ;; Remove the slow recursive finder
  (cl-callf2 remq 'pet-find-file-from-project-root-recursively pet-find-file-functions))


;; Mount/umount gocryptfs vaults from Emacs
(use-package gocryptfs
  :straight (:host github :repo "abougouffa/emacs-gocryptfs")
  :when (or (featurep 'os/linux) (featurep 'os/bsd))
  :bind (:map minemacs-open-thing-map ("e" . gocryptfs-toggle-mount)))


;; Package manager for LSPs, DAPs, linters, and more
(use-package mason
  :straight t
  :defer 2
  :hook (minemacs-build-functions . mason-update-registry)
  :config
  (+shutup! :log (mason-ensure)))


(provide 'me-tools)

;;; me-tools.el ends here
