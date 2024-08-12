;;; me-json.el --- JSON language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-json
  :auto-mode '((("\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'" "/.babelrc\\'" "/.bowerrc\\'" "/composer.lock\\'") . json-mode))
  :companion-packages '((json-ts-mode . (jq-mode json-mode))
                        ;; To use the custom `+yq-interactively' and `+xq-interactively'
                        ((nxml-mode yaml-mode yaml-ts-mode) . jq-mode)))

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


(provide 'on-demand/me-json)
;;; me-json.el ends here
