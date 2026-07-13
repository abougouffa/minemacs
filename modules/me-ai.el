;; me-ai.el --- AI assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-01-25
;; Last modified: 2026-07-13

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
  (claude-code-ide-terminal-backend 'ghostel)
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

  (claude-code-ide-emacs-tools-setup))


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
