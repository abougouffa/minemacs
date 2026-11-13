;;; me-ellama.el --- LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2022-202  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-13
;; Last modified: 2026-11-13

;;; Commentary:

;;; Code:


;; A package for interacting with LLMs from Emacs
(use-package ellama
  :straight t
  :autoload ellama-get-ollama-model-names ellama-get-ollama-chat-model-names ellama-get-ollama-embedding-model-names)


(provide 'obsolete/me-ellama)
;;; me-ellama.el ends here
