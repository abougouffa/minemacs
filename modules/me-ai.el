;; me-ai.el --- AI assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-01-25
;; Last modified: 2026-01-26

;;; Commentary:

;;; Code:

;; A library abstracting LLM capabilities for Emacs
(use-package llm
  :straight t)


;; `llm' module for integrating with Ollama
(use-package llm-ollama
  :straight llm
  :autoload make-llm-ollama)


;; A package for interacting with LLMs from Emacs
(use-package ellama
  :straight t
  :autoload ellama-get-ollama-model-names ellama-get-ollama-chat-model-names ellama-get-ollama-embedding-model-names)


;; Pair-programming with AI agents using Aider
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :custom
  (aidermacs-default-model ; Get the first available model
   (cl-loop for model in (ellama-get-ollama-model-names)
            when (cl-member model '("devstral" "qwen2.5-coder" "deepseek-coder-v2" "command-r7b"
                                    "codellama" "codegemma" "codestral" "codegeex4"
                                    "starcoder2" "granite-code" "mistral-nemo"
                                    "mistral-small" "deepseek-r1")
                            :test (lambda (a b) (string-prefix-p b a)))
            return (concat "ollama_chat/" model)))
  :config
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434"))


;; Speech-to-Text interface for Emacs using OpenAI's whisper model and whisper.cpp as inference engine
(use-package whisper
  :straight (:host github :repo "natrys/whisper.el")
  :custom
  (whisper-install-directory (+directory-ensure minemacs-local-dir "whisper/"))
  (whisper-use-threads (/ (num-processors) 2))
  (whisper-model "medium")
  (whisper-language "auto"))


(provide 'me-ai)

;;; me-ai.el ends here
