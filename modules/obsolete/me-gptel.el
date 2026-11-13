;;; me-gptel.el --- LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2022-202  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-13
;; Last modified: 2026-11-13

;;; Commentary:

;;; Code:


;; Interact with LLMs
(use-package gptel
  :straight t
  :config
  (defvar +gptel-llama-cpp-models '(ggml-org/Ministral-3-8B-Instruct-2512-GGUF))
  (defun +gptel-llama-cpp-setup (model)
    "Set `gptel' to use a llama.cpp MODEL."
    (interactive (list (make-symbol (completing-read "Select a model: " +gptel-llama-cpp-models))))
    (setopt gptel-model model
            gptel-backend (gptel-make-openai "llama-cpp"
                            :stream t
                            :protocol "http"
                            :host "localhost:8080"
                            :models (cl-remove-duplicates (cons model +gptel-llama-cpp-models)))))
  (setopt gptel-model 'mistral:latest
          gptel-backend (gptel-make-ollama "Ollama" :host "localhost:11434" :stream t :models '(mistral:latest))))


(provide 'obsolete/me-gptel)
;;; me-gptel.el ends here
