;;; me-yaml.el --- YAML language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-yaml
  :auto-mode '(("\\.\\(e?ya?\\|ra\\)ml\\'" . yaml-mode))
  :magic-mode '(("^%YAML\\s-+[0-9]+\\.[0-9]+\\(\\s-+#\\|\\s-*$\\)" . yaml-mode))
  :companion-packages '((yaml-ts-mode . (yaml-mode yaml-pro-ts-mode ansible-mode))))


;; Major mode for editing YAML files
(use-package yaml-mode
  :straight t
  :mode "Procfile\\'"
  :mode (rx (any ?. ?_) (or "clang-format" "clang-tidy") eol))


;; Parser-aided YAML editing features
(use-package yaml-pro
  :straight t
  :when (+emacs-options-p 'tree-sitter)
  :hook (yaml-mode . yaml-pro-mode)
  :hook (yaml-ts-mode . yaml-pro-ts-mode))


;; Ansible minor mode
(use-package ansible
  :straight (ansible :nonrecursive t))


(provide 'on-demand/me-yaml)
;;; me-yaml.el ends here
