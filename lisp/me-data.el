;;; data-files.el --- Data files (csv, yaml, xml, ...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

(use-package csv-mode
  :straight t
  :mode ("\\.csv\\'" . csv-mode)
  :general
  (me-local-def :keymaps 'csv-mode-map
    "r" '(me-csv-rainbow :which-key "CSV Rainbow")
    "a" #'csv-align-fields
    "u" #'csv-unalign-fields
    "s" #'csv-sort-fields
    "S" #'csv-sort-numeric-fields
    "k" #'csv-kill-fields
    "t" #'csv-transpose)
  :config
  ;; TODO: Need to fix the case of two commas, example "a,b,,c,d"
  (defun me-csv-rainbow (&optional separator)
    (require 'cl-lib)
    (require 'color)
    (interactive (list (when current-prefix-arg (read-char "Separator: "))))
    (font-lock-mode 1)
    (let* ((separator (or separator ?\,))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                            collect (apply #'color-rgb-to-hex
                                           (color-hsl-to-rgb i 0.3 0.5)))))
      (cl-loop for i from 2 to n by 2
               for c in colors
               for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
               do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c))))))))))


(use-package yaml-mode
  :straight t
  :mode "Procfile\\'")


(use-package json-mode
  :straight t
  :mode "\\.js\\(?:on\\|[hl]int\\(?:rc\\)?\\)\\'"
  :general
  (me-local-def :keymaps 'json-mode-map
    "p" #'json-mode-show-path
    "t" #'json-toggle-boolean
    "d" #'json-mode-kill-path
    "x" #'json-nullify-sexp
    "+" #'json-increment-number-at-point
    "-" #'json-decrement-number-at-point
    "f" #'json-mode-beautify))


(provide 'me-data)
