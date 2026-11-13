;;; me-llm.el --- LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2022-202, 026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-13
;; Last modified: 2026-11-13

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


(provide 'obsolete/me-llm)
;;; me-llm.el ends here
