;;; me-web.el --- Web development modes -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-web
  :auto-mode '((("\\.[px]?html?\\'"
                 "\\.tpl\\.php\\'"
                 "\\.[lh]?eex\\'"
                 "\\.[agj]sp\\'"
                 "\\.ejs\\'"
                 "\\.hbs\\'"
                 "\\.svelte\\'"
                 "\\.twig\\'"
                 "\\.jinja2?\\'"
                 "\\.eco\\'"
                 "\\.as[cp]x\\'"
                 "\\.erb\\'"
                 "\\.mustache\\'"
                 "\\.djhtml\\'"
                 "\\.vue\\'"
                 "wp-content/themes/.+/.+\\.php\\'"
                 "\\.\\(?:tpl\\|blade\\)\\(?:\\.php\\)?\\'"
                 "/\\(views\\|html\\|templates\\)/.*\\.php\\'")
                . web-mode)
               ("\\.haml\\'" . haml-mode)
               ("\\.sass\\'" . sass-mode)
               ("\\.was?t\\'" . wat-mode))
  :companion-packages '(((php-mode php-ts-mode html-mode html-ts-mode css-mode) . (web-mode haml-mode emmet-mode sass-mode))
                        ((js-mode js-ts-mode) . flymake-biome)))

(use-package web-mode
  :straight t
  :custom
  (web-mode-enable-html-entities-fontification t)
  :config
  (add-to-list 'web-mode-engines-alist '("elixir" . "\\.eex\\'"))
  (add-to-list 'web-mode-engines-alist '("phoenix" . "\\.[lh]eex\\'")))

(use-package emmet-mode
  :straight t)

(use-package haml-mode
  :straight t)

(use-package sass-mode
  :straight t)

(use-package wat-mode
  :straight (:host github :repo "devonsparks/wat-mode")
  :commands (wat-mode))

(use-package flymake-biome
  :straight t)


(provide 'obsolete/me-web)
;;; me-web.el ends here
