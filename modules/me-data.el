;;; me-data.el --- Data and vizualizaion formats (csv, yaml, xml, graphviz, ...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package csv-mode
  :straight t
  :hook (csv-mode . csv-guess-set-separator)
  :custom
  (csv-separators '("," ";" "\t" "|")))

(use-package rainbow-csv
  :straight (:host github :repo "emacs-vs/rainbow-csv"))

(use-package yaml-mode
  :straight t
  :mode "Procfile\\'"
  :mode (rx (any ?. ?_) (or "clang-format" "clang-tidy") eol))

(use-package yaml-pro
  :straight t
  :when (+emacs-features-p 'tree-sitter)
  :hook (yaml-mode . yaml-pro-mode)
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(use-package toml-mode
  :straight t)

(use-package json-mode
  :straight t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'")

(use-package jq-mode
  :straight t
  :custom
  (jq-interactive-font-lock-mode (if (+emacs-features-p 'tree-sitter) #'json-ts-mode #'json-mode))
  :commands (+yq-interactively +xq-interactively)
  :config
  (defun +yq-interactively ()
    "Use `jq-interactively' with \"yq\" for YAML."
    (interactive)
    (let ((jq-interactive-command "yq")
          (jq-interactive-default-prompt "yq: ")
          (jq-interactive-font-lock-mode (if (+emacs-features-p 'tree-sitter) #'yaml-ts-mode #'yaml-mode))
          (jq-interactive-default-options "")) ;; "--yaml-roundtrip"
      (call-interactively #'jq-interactively)))

  (defun +xq-interactively ()
    "Use `jq-interactively' with \"xq\" for XML."
    (interactive)
    (let ((jq-interactive-command "xq")
          (jq-interactive-default-prompt "xq: ")
          (jq-interactive-font-lock-mode #'nxml-mode)
          (jq-interactive-default-options "--node -x"))
      (call-interactively #'jq-interactively))))


(provide 'me-data)

;;; me-data.el ends here
