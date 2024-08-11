;;; me-graphviz.el --- Graphviz -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-graphviz
  :auto-mode '(("\\.gv\\'" . graphviz-dot-mode) ("\\.dot\\'" . graphviz-dot-mode)))

(use-package graphviz-dot-mode
  :straight (:files ("graphviz-dot-mode.el" "texinfo"))
  :custom
  (graphviz-dot-view-command "xdot %s")
  (graphviz-dot-preview-extension "svg")
  :config
  (+eglot-register 'graphviz-dot-mode '("dot-language-server" "--stdio")))


(provide 'on-demand/me-graphviz)
;;; me-graphviz.el ends here
