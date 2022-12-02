;; -*- lexical-binding: t; -*-

;; Use external tree-sitter package if not included in Emacs
(use-package tree-sitter
  :straight t
  :hook ((python-mode
          c-mode
          c++-mode
          csharp-mode
          rust-mode
          json-mode
          xml-mode
          sh-mode
          typescript-ts-mode
          js-mode) . tree-sitter-mode))

(use-package tree-sitter-hl
  :hook (tree-sitter-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :config
  (tree-sitter-langs-install-grammars t))

(use-package ts-fold
  :straight (:type git :host github :repo "emacs-tree-sitter/ts-fold")
  :hook (prog-mode . +ts-fold-ensure)
  :init
  (defun +ts-fold-ensure ()
    (if (derived-mode-p 'emacs-lisp-mode)
        (hs-minor-mode) ;; Fall back to builtin
      (ts-fold-mode))))

;; Needed by `ts-fold'
(use-package fringe-helper
  :straight t)


(provide 'me-external-tree-sitter)
