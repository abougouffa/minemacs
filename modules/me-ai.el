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
                                          ((featurep 'eat) 'eat))))


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
