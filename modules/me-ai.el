;; me-ai.el --- AI assistants -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-01-25
;; Last modified: 2025-05-24

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
  (defconst +ollama-embedding-models '("paraphrase-multilingual" "granite-embedding"
                                       "nomic-embed-text" "mxbai-embed-large"
                                       "bge-m3" "bge-large" "all-minilm"
                                       "snowflake-arctic-embed" "snowflake-arctic-embed2"))
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
  :config
  (defun +ellama-set-available-providers ()
    "Automatically set the available providers from Ollama."
    (interactive)
    (when-let* ((models (+ollama-list-installed-models)))
      (let ((embedding-models (+ollama-list-installed-embedding-models)))
        (setopt ellama-providers
                (cl-loop for model in models
                         unless (member model embedding-models)
                         collect (cons model (make-llm-ollama
                                              :chat-model model
                                              :embedding-model (+ollama-get-default-embedding-model))))
                ellama-provider (cdr (car ellama-providers))))))

  ;; Ensure loading all the available providers from Ollama
  (ignore-errors (+ellama-set-available-providers)))


;; Emacs Lisp Information System Assistant, LLM-based information agent leveraging a Retrieval Augmented Generation (RAG) approach
(use-package elisa
  :straight t
  :hook (minemacs-build-functions . elisa-download-sqlite-vss)
  :config
  (require 'ellama)
  (require 'llm-ollama)
  (defun +elisa-set-providers-from-ellama ()
    (setopt elisa-embeddings-provider (make-llm-ollama :embedding-model (+ollama-get-default-embedding-model))
            elisa-chat-provider (make-llm-ollama :chat-model (copy-sequence ellama-provider)
                                                 :embedding-model (+ollama-get-default-embedding-model))))

  (advice-add 'ellama-provider-select :after #'+elisa-set-providers-from-ellama)
  (+elisa-set-providers-from-ellama))


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
  (whisper-language "auto")
  :config
  (defvar +whisper-languages '("auto" "fr" "en" "ar"))
  (defun +whisper-change-language (lang)
    "Select the language LANG."
    (interactive (list (completing-read "Select language: " +whisper-languages)))
    (setopt whisper-language lang)))


(provide 'me-ai)

;;; me-ai.el ends here
