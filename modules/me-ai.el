;; me-ai.el --- AI assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package llm-ollama
  :straight llm
  :autoload make-llm-ollama)

(use-package ellama
  :straight t
  :init
  (defvar +ellama-process-name "ellama-server")
  (defvar +ellama-server-buffer-name " *ellama-server*")
  (defun +ellama-serve ()
    "Start Ellama server."
    (interactive)
    (if (executable-find "ollama")
        (if (get-process +ellama-process-name)
            (message "The Ollama server is already running, call `+ellama-kill-server' to stop it.")
          (if (make-process :name +ellama-process-name :buffer +ellama-server-buffer-name :command '("ollama" "serve"))
              (message "Successfully started Ollama server.")
            (user-error "Cannot start the Ollama server")))
      (user-error "Cannot find the \"ollama\" executable")))
  (defun +ellama-kill-server ()
    "Kill Ellama server."
    (interactive)
    (let ((ollama (get-process +ellama-process-name)))
      (if ollama
          (if (kill-process ollama)
              (message "Killed Ollama server.")
            (user-error "Cannot kill the Ollama server"))
        (message "No running Ollama server."))))
  :custom
  (ellama-provider (make-llm-ollama :chat-model "llama2:13b" :embedding-model "llama2:13b")))


(provide 'me-ai)

;;; me-ai.el ends here
