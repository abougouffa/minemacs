;;; me-graphviz.el --- Graphviz -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-graphviz
  :auto-mode '(("\\.gv\\'" . graphviz-dot-mode) ("\\.dot\\'" . graphviz-dot-mode)))


;; Mode for the dot-language used by GraphViz
(use-package graphviz-dot-mode
  :straight (:files ("graphviz-dot-mode.el" "texinfo"))
  :custom
  (graphviz-dot-view-command "xdot %s")
  (graphviz-dot-preview-extension "svg")
  :config
  (+eglot-register 'graphviz-dot-mode '("dot-language-server" "--stdio")))


(provide 'on-demand/me-graphviz)
;;; me-graphviz.el ends here
