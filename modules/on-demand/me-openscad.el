;;; me-openscad.el --- OpenSCAD -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-openscad
  :auto-mode '(("\\.scad\\'" . scad-mode)))


;; A major mode for editing OpenSCAD code
(use-package scad-mode
  :ensure t
  :init
  (with-eval-after-load 'nerd-icons
    (add-to-list
     'nerd-icons-extension-icon-alist
     '("scad" nerd-icons-mdicon "nf-md-cad_file" :face nerd-icons-yellow)))
  :config
  (with-eval-after-load 'apheleia-formatters
    (push '(scad-mode . clang-format) apheleia-mode-alist))
  (+eglot-register 'scad-mode '("openscad-lsp" "--stdio")))


(provide 'on-demand/me-openscad)
;;; me-openscad.el ends here
