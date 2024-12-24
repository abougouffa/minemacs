;; me-ai.el --- AI assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; A library abstracting LLM capabilities for Emacs
(use-package llm
  :straight t)


;; `llm' module for integrating with Ollama
(use-package llm-ollama
  :straight llm
  :autoload make-llm-ollama +ollama-serve +ollama-kill-server +ollama-list-installed-models
  :config
  (defvar +ollama-process-name "ellama-server")
  (defvar +ollama-server-buffer-name " *ellama-server*")

  (defun +ollama-serve ()
    "Start Ellama server."
    (interactive)
    (if (executable-find "ollama")
        (if (get-process +ollama-process-name)
            (message "The Ollama server is already running, call `+ollama-kill-server' to stop it.")
          (if (make-process :name +ollama-process-name :buffer +ollama-server-buffer-name :command '("ollama" "serve"))
              (message "Successfully started Ollama server.")
            (user-error "Cannot start the Ollama server"))
          (with-eval-after-load 'ellama (+ellama-set-providers)))
      (user-error "Cannot find the \"ollama\" executable")))

  (defun +ollama-kill-server ()
    "Kill Ellama server."
    (interactive)
    (let ((ollama (get-process +ollama-process-name)))
      (if ollama
          (if (kill-process ollama)
              (message "Killed Ollama server.")
            (user-error "Cannot kill the Ollama server"))
        (message "No running Ollama server."))))

  (defun +ollama-list-installed-models ()
    "Return the installed models"
    (let* ((ret (shell-command-to-string "ollama list"))
           (models (cdr (string-lines ret))))
      (if (and (string-match-p "NAME[[:space:]]*ID[[:space:]]*SIZE[[:space:]]*MODIFIED" ret) (length> models 0))
          (mapcar (lambda (m) (car (string-split m))) models)
        (user-error "Cannot detect installed models, please make sure Ollama server is started")))))


;; A package for interacting with LLMs from Emacs
(use-package ellama
  :straight t
  :config
  (defun +ellama-set-providers ()
    (setopt ellama-providers
            (cl-loop for model in (+ollama-list-installed-models)
                     collect (cons model (make-llm-ollama :chat-model model :embedding-model model)))
            ellama-provider (cdr (car ellama-providers)))))


;; Emacs Lisp Information System Assistant, LLM-based information agent leveraging a Retrieval Augmented Generation (RAG) approach
(use-package elisa
  :straight t)


;; Speech-to-Text interface for Emacs using OpenAI's whisper model and whisper.cpp as inference engine
(use-package whisper
  :straight (:host github :repo "natrys/whisper.el")
  :custom
  (whisper-install-directory (+directory-ensure minemacs-local-dir "whisper/"))
  (whisper-use-threads (/ (num-processors) 2)))


(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el")
  :custom
  (copilot-install-dir (expand-file-name "copilot" minemacs-local-dir)))


(provide 'me-ai)

;;; me-ai.el ends here
