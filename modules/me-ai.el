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
  :autoload make-llm-ollama +ollama-list-installed-models
  :config
  (defconst +ollama-embedding-models
    '("paraphrase-multilingual" "granite-embedding" "nomic-embed-text" "mxbai-embed-large"
      "qwen3-embedding" "bge-m3" "bge-large" "all-minilm" "embeddinggemma" "snowflake-arctic-embed"))
  (defvar +ollama-prefer-embedding-model nil) ; See https://ollama.com/search?c=embedding

  (defun +ollama-list-installed-models ()
    "Return the installed models"
    (if (zerop (call-process-shell-command "ollama ps"))
        (let* ((ret (shell-command-to-string "ollama list"))
               (models (cdr (string-lines ret))))
          (if (and (string-match-p "NAME[[:space:]]*ID[[:space:]]*SIZE[[:space:]]*MODIFIED" ret) (length> models 0))
              (mapcar (lambda (m) (car (string-split m))) models)
            (user-error "No model available, please pull some Ollama model")))
      (user-error "Please make sure Ollama server is started")))

  (defun +ollama-list-installed-embedding-models ()
    (seq-intersection (+ollama-list-installed-models) +ollama-embedding-models #'string-prefix-p))

  (defun +ollama-get-default-embedding-model ()
    (let ((embedding-models (+ollama-list-installed-embedding-models)))
      (or (and +ollama-prefer-embedding-model
               (cl-find +ollama-prefer-embedding-model embedding-models :test #'string-prefix-p))
          (car embedding-models)
          (car (+ollama-list-installed-models))))))


;; A package for interacting with LLMs from Emacs
(use-package ellama
  :straight t
  :autoload ellama-get-ollama-model-names ellama-get-ollama-chat-model-names ellama-get-ollama-embedding-model-names)


;; Pair-programming with AI agents using Aider
(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs")
  :custom
  (aidermacs-default-model ; Get the first available model
   (cl-loop for model in (+ollama-list-installed-models)
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
