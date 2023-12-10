;;; me-data.el --- Data and visualizaion formats (csv, yaml, xml, graphviz, ...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa and contributors

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package csv-mode
  :straight t
  :config
  (+map-local! :keymaps 'csv-mode-map
    "r" #'+csv-rainbow
    "a" #'csv-align-fields
    "u" #'csv-unalign-fields
    "s" #'csv-sort-fields
    "S" #'csv-sort-numeric-fields
    "k" #'csv-kill-fields
    "t" #'csv-transpose)

  ;; Adapted from: reddit.com/r/emacs/comments/26c71k/comment/chq2r8m
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

(use-package yaml-mode
  :straight t
  :mode "Procfile\\'")

(use-package yaml-pro
  :straight t
  :when (+emacs-features-p 'tree-sitter)
  :hook (yaml-mode . yaml-pro-mode)
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(use-package toml-mode
  :straight t)

(use-package json-mode
  :straight t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :config
  (+map-local! :keymaps '(json-mode-map json-ts-mode-map)
    "p" #'json-mode-show-path
    "t" #'json-toggle-boolean
    "d" #'json-mode-kill-path
    "x" #'json-nullify-sexp
    "+" #'json-increment-number-at-point
    "-" #'json-decrement-number-at-point
    "f" #'json-mode-beautify))

(use-package graphviz-dot-mode
  :straight (graphviz-dot-mode :files ("graphviz-dot-mode.el" "texinfo"))
  :custom
  (graphviz-dot-view-command "xdot %s")
  (graphviz-dot-preview-extension "svg")
  :config
  (+map-local! :keymaps 'graphviz-dot-mode-map
    "p" #'graphviz-dot-preview
    "P" #'graphviz-dot-view
    "l" #'graphviz-turn-on-live-preview
    "L" #'graphviz-turn-off-live-preview)
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
    (add-to-list 'completion-at-point-functions #'+plantuml-completion-at-point))

  (+map-local! :keymaps 'plantuml-mode-map
    "p" #'plantuml-preview-buffer
    "P" #'plantuml-preview
    "d" `(,(+cmdfy! (if plantuml-mode-debug-enabled
                        (plantuml-disable-debug)
                      (plantuml-enable-debug)))
          :wk "Toggle debug")))

(use-package mermaid-mode
  :straight t
  :config
  (+map-local! :keymaps 'mermaid-mode-map
    "c" 'mermaid-compile
    "f" 'mermaid-compile-file
    "b" 'mermaid-compile-buffer
    "r" 'mermaid-compile-region
    "b" 'mermaid-open-browser
    "d" 'mermaid-open-doc))

(use-package ob-mermaid
  :straight (:host github :repo "arnm/ob-mermaid")
  :after minemacs-first-org-file ob
  :demand t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append org-babel-load-languages '((mermaid . t)))))

(use-package d2-mode
  :straight t
  :mode "\\.d2\\'"
  :config
  (+map-local! :keymaps 'd2-mode-map
    "cc" #'d2-compile
    "cf" #'d2-compile-file
    "cb" #'d2-compile-buffer
    "cr" #'d2-compile-region
    "cF" #'d2-compile-file-and-browse
    "cB" #'d2-compile-buffer-and-browse
    "cR" #'d2-compile-region-and-browse
    "o"  #'d2-open-browser
    "v"  #'d2-view-current-svg
    "h"  #'d2-open-doc))

(use-package ob-d2
  :straight t
  :after minemacs-first-org-file ob
  :demand t
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
