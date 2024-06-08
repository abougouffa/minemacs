;;; me-data.el --- Data and vizualizaion formats (csv, yaml, xml, graphviz, ...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package csv-mode
  :straight t)

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
  :commands +yq-interactively +xq-interactively
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

(use-package graphviz-dot-mode
  :straight (:files ("graphviz-dot-mode.el" "texinfo"))
  :custom
  (graphviz-dot-view-command "xdot %s")
  (graphviz-dot-preview-extension "svg")
  :config
  (+eglot-register 'graphviz-dot-mode '("dot-language-server" "--stdio")))

(use-package plantuml-mode
  :straight t
  :hook (plantuml-mode . +plantuml-mode-setup)
  :custom
  (plantuml-jar-path (concat minemacs-local-dir "plantuml/plantuml.jar"))
  (plantuml-indent-level 2)
  :config
  ;; Define `capf' function, based on `plantuml-complete-symbol'
  (defun +plantuml-completion-at-point ()
    "Perform symbol-at-pt completion on word before cursor."
    (when (derived-mode-p 'plantuml-mode) ; do not fire up on other modes
      (let* ((end-pos (point))
             (sym-at-pt (or (thing-at-point 'symbol) ""))
             (max-match (try-completion sym-at-pt plantuml-kwdList)))
        (unless (null max-match)
          (list (- end-pos (length sym-at-pt))
                end-pos
                (if (eq max-match t)
                    (list keyword)
                  (all-completions sym-at-pt plantuml-kwdList)))))))

  ;; Add support for `capf'
  (defun +plantuml-mode-setup ()
    (add-to-list 'completion-at-point-functions #'+plantuml-completion-at-point)))

(use-package mermaid-mode
  :straight t)

(use-package ob-mermaid
  :straight (:host github :repo "arnm/ob-mermaid")
  :after minemacs-first-org-file ob
  :demand
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((mermaid . t)))))

(use-package d2-mode
  :straight t
  :mode "\\.d2\\'")

(use-package ob-d2
  :straight t
  :after minemacs-first-org-file ob
  :demand
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((d2 . t)))))

(use-package gnuplot
  :straight t
  :mode ("\\.gnuplot\\'" . gnuplot-mode)
  :hook (gnuplot-mode . display-line-numbers-mode)
  :hook (gnuplot-mode . visual-line-mode))


(provide 'me-data)

;;; me-data.el ends here
