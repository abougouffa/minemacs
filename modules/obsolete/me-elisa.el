;;; me-elisa.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-02-01
;; Last modified: 2026-02-01

;;; Commentary:

;;; Code:


;; Emacs Lisp Information System Assistant, LLM-based information agent leveraging a Retrieval Augmented Generation (RAG) approach
(use-package elisa
  :straight t
  :hook (minemacs-build-functions . elisa-download-sqlite-vss)
  :config
  (require 'ellama)
  (require 'llm-ollama)
  (defun +elisa-set-providers-from-ellama ()
    (when-let* ((chat (car (ellama-get-ollama-chat-model-names)))
                (embed (car (ellama-get-ollama-embedding-model-names))))
      (setopt elisa-embeddings-provider (make-llm-ollama :embedding-model embed)
              elisa-chat-provider (make-llm-ollama :chat-model (or (copy-sequence ellama-provider) chat) :embedding-model embed))))

  (advice-add 'ellama-provider-select :after #'+elisa-set-providers-from-ellama)
  (+elisa-set-providers-from-ellama))



(provide 'obsolete/me-elisa)
;;; me-elisa.el ends here
