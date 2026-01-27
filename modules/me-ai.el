;; me-ai.el --- AI assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-01-25
;; Last modified: 2026-01-27

;;; Commentary:

;;; Code:

;; A library abstracting LLM capabilities for Emacs
(use-package llm
  :straight t)


;; `llm' module for integrating with Ollama
(use-package llm-ollama
  :straight llm
  :autoload make-llm-ollama)


(use-package llm-models
  :config
  (cl-callf2 append ; Add some missing Ollama models
      ;; Add to the beginning, otherwise, "qwen3-embedding" will match the "qwen3" when searching for embedding models
      (list (make-llm-model
             :name "Embedding (Gemma)" :symbol 'embeddinggemma
             :capabilities '(embedding free-software)
             :context-length 2048
             :regex "embeddinggemma")
            (make-llm-model
             :name "Embedding (Qwen 3)" :symbol 'qwen3-embedding
             :capabilities '(embedding free-software)
             :context-length 32768
             :regex "qwen3-embedding")
            (make-llm-model
             :name "Embedding (Granite)" :symbol 'granite-embedding
             :capabilities '(embedding free-software)
             :context-length 512
             :regex "granite-embedding")
            (make-llm-model
             :name "BGE-Large" :symbol 'bge-large
             :capabilities '(embedding free-software)
             :context-length 512
             :regex "bge-large")
            (make-llm-model
             :name "Nomic v2 MoE" :symbol 'nomic-embed-text-v2-moe
             :capabilities '(embedding free-software)
             :context-length 512
             :regex "nomic-embed-text-v2-moe")
            (make-llm-model
             :name "Embedding (Paraphrase Multilingual)" :symbol 'paraphrase-multilingual
             :capabilities '(embedding free-software)
             :context-length 512
             :regex "paraphrase-multilingual"))
      llm-models))


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
  (setenv "OLLAMA_API_BASE" (concat "http://" (or (getenv "OLLAMA_HOST") "127.0.0.1:11434"))))


;; Integration for Model Context Protocol (MCP)
(use-package mcp-hub
  :straight mcp
  :custom
  (mcp-hub-servers `(("ddg" . (:command "uvx" :args ("duckduckgo-mcp-server")))))
  :config
  (mcp-hub-start-all-server
   (lambda ()
     (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
       (mapcar (lambda (tool) (apply #'ellama-tools-define-tool)) (mapcar #'list tools))))))


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
