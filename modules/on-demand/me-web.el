;;; me-web.el --- Web development modes -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-05-04

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-web
  :auto-mode '((("\\.tpl\\.php\\'" "\\.[lh]?eex\\'" "\\.[agj]sp\\'" "\\.ejs\\'"
                 "\\.hbs\\'" "\\.svelte\\'" "\\.twig\\'" "\\.jinja2?\\'"
                 "\\.eco\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'"
                 "\\.djhtml\\'" "\\.vue\\'" "wp-content/themes/.+/.+\\.php\\'"
                 "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'" "/\\(views\\|html\\|templates\\)/.*\\.php\\'")
                . web-mode)
               ("\\.haml\\'" . haml-mode)
               ("\\.sass\\'" . sass-mode)
               ("\\.was?t\\'" . wat-mode))
  :companion-packages '(((php-mode php-ts-mode html-mode html-ts-mode css-mode) . (web-mode haml-mode emmet-mode sass-mode))
                        ((js-mode js-ts-mode) . flymake-biome)))


;; Major mode for editing web templates
(use-package web-mode
  :straight t
  :custom
  (web-mode-enable-html-entities-fontification t)
  :config
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.[lh]eex\\'")))


;; Support for Emmet, the essential toolkit for web-developers
(use-package emmet-mode
  :straight t)


;; Major mode for editing Haml files
(use-package haml-mode
  :straight t)


;; Major mode for editing Sass files
(use-package sass-mode
  :straight t)


;; Major mode for WebAssembly
(use-package wat-mode
  :straight (:host github :repo "devonsparks/wat-mode")
  :commands (wat-mode))


;; Flymake integration for checking JavaScript files using `biome'
(use-package flymake-biome
  :straight t)


(provide 'on-demand/me-web)
;;; me-web.el ends here
