;;; me-aidermacs.el --- LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2022-202  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-13
;; Last modified: 2026-11-13

;;; Commentary:

;;; Code:


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


(provide 'obsolete/me-aidermacs)
;;; me-aidermacs.el ends here
