;;; me-data.el --- Data files (csv, yaml, xml, ...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package csv-mode
  :straight t
  :mode "\\.csv\\'"
  :config
  (+map-local :keymaps 'csv-mode-map
    "r" #'+csv-rainbow
    "a" #'csv-align-fields
    "u" #'csv-unalign-fields
    "s" #'csv-sort-fields
    "S" #'csv-sort-numeric-fields
    "k" #'csv-kill-fields
    "t" #'csv-transpose)

  ;; Adapted from: https://reddit.com/r/emacs/comments/26c71k/comment/chq2r8m
  (defun +csv-rainbow (&optional separator)
    "Colorize CSV columns."
    (interactive (list (when current-prefix-arg (read-char "Separator: "))))
    (require 'color)
    (font-lock-mode 1)
    (let* ((separator (or separator ?\,))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                            collect (apply #'color-rgb-to-hex
                                           (color-hsl-to-rgb i 0.3 0.5)))))
      (cl-loop for i from 2 to (1+ n) by 2
               for c in colors
               for r = (format "^\\([^%c\n]*[%c\n]\\)\\{%d\\}" separator separator i)
               do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c))))))))))

(use-package yaml-ts-mode
  :straight (:type built-in)
  :when (>= emacs-major-version 29)
  :mode "Procfile\\'")

(use-package yaml-pro
  :straight t
  :hook (yaml-mode . yaml-pro-mode)
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package json-mode
  :straight t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (+map-local :keymaps 'json-mode-map
    "p" #'json-mode-show-path
    "t" #'json-toggle-boolean
    "d" #'json-mode-kill-path
    "x" #'json-nullify-sexp
    "+" #'json-increment-number-at-point
    "-" #'json-decrement-number-at-point
    "f" #'json-mode-beautify))

(use-package graphviz-dot-mode
  :straight (graphviz-dot-mode :files ("graphviz-dot-mode.el" "texinfo"))
  :defer t
  :general
  (+map-local :keymaps 'graphviz-dot-mode-map
    "p" #'graphviz-dot-preview
    "P" #'graphviz-dot-view
    "l" #'graphviz-turn-on-live-preview
    "L" #'graphviz-turn-off-live-preview)
  :custom
  (graphviz-dot-view-command "xdot %s")
  (graphviz-dot-preview-extension "svg")
  :config
  (+eglot-register 'graphviz-dot-mode '("dot-language-server" "--stdio")))

(use-package nxml-mode
  :straight (:type built-in)
  :defer t
  :config
  (+eglot-register '(nxml-mode xml-mode) "lemminx"))


(provide 'me-data)
