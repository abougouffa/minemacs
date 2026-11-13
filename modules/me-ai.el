;; me-ai.el --- AI assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-01-25
;; Last modified: 2026-11-13

;;; Commentary:

;;; Code:


;; Emacs Web Server
(use-package web-server
  ;; HACK+FIX: The `simple-httpd' package, installed as a dependency of other
  ;; packages, have the same repository name "emacs-web-server", which conflicts
  ;; with this package's repo, so we change the local repo to "web-server.el"
  :straight (:host github :repo "eschulte/emacs-web-server" :local-repo "eschulte-web-server"))


;; Claude Code IDE integration for Emacs
(use-package claude-code-ide
  :straight (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-diagnostics-backend 'flymake)
  (claude-code-ide-terminal-backend (cond ((featurep 'ghostel) 'ghostel)
                                          ((featurep 'eat) 'eat)))
  :config
  (require 'web-server) ; Needed by `claude-code-ide' for its MCP server
  ;; TWEAK+FIX: A hack to make it work with Claude that runs inside a sandboxed
  ;; DevContainer
  (when (require 'devcontainer nil t)
    (defun +claude-code-ide--detect-cli-devcontainer ()
      "Detect if Claude Code CLI is available."
      (let ((available (condition-case nil
                           (let ((args (devcontainer-advise-command (list claude-code-ide-cli-path "--version"))))
                             (zerop (apply #'call-process `(,(car args) nil nil nil ,@(cdr args)))))
                         (error nil))))
        (setq claude-code-ide--cli-available available)))

    (defun +claude-code-ide--build-claude-command-devcontainer (fn &rest fn-args)
      (let* ((args (devcontainer-advise-command (ensure-list claude-code-ide-cli-path)))
             (args `(,(car args) ,(cadr args) "-it" ,@(cddr args)))
             (claude-code-ide-cli-path (string-join args " ")))
        (apply fn fn-args)))

    (advice-add 'claude-code-ide--detect-cli :override #'+claude-code-ide--detect-cli-devcontainer)
    (advice-add 'claude-code-ide--build-claude-command :around #'+claude-code-ide--build-claude-command-devcontainer))

  ;; Inspired by Spacemacs
  (defun +claude-code-explain-at-point ()
    "Send a question about the symbol/region at point to Claude Code."
    (interactive)
    (if-let* ((symbol (or (thing-at-point 'region t) (thing-at-point 'symbol t))))
        (claude-code-ide-send-prompt (format "What does %s do?" symbol))
      (message "No symbol at point")))

  (claude-code-ide-emacs-tools-setup))


;; Speech-to-Text interface for Emacs using OpenAI's whisper model and whisper.cpp as inference engine
(use-package whisper
  :straight (:host github :repo "natrys/whisper.el")
  :custom
  (whisper-install-directory (+directory-ensure minemacs-local-dir "whisper/"))
  (whisper-use-threads (/ (num-processors) 2))
  (whisper-model "medium")
  (whisper-language "auto")
  :config
  (when (require 'nerd-icons nil t)
    (setq whisper--mode-line-recording-indicator (concat (+nerd-icons-icon "nf-md-microphone" :face 'font-lock-warning-face) " ")
          whisper--mode-line-transcribing-indicator (concat (+nerd-icons-icon "nf-md-transcribe" :face 'font-lock-warning-face) " "))))


(provide 'me-ai)

;;; me-ai.el ends here
