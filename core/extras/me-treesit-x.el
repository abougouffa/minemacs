;;; me-treesit-x.el --- Extra tweaks Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-11
;; Last modified: 2025-08-31

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'treesit (require 'me-treesit-x))

(require 'treesit)

(defvar +treesit-create-parser-modes-deny nil)

(defvar +treesit-builtin-feature-mode-alist
  '((java-ts-mode) (markdown-ts-mode) (json-ts-mode) (go-ts-mode) (lua-ts-mode)
    (php-ts-mode) (heex-ts-mode) (html-ts-mode) (ruby-ts-mode) (toml-ts-mode)
    (yaml-ts-mode) (rust-ts-mode) (mhtml-ts-mode) (cmake-ts-mode) (elixir-ts-mode)
    (dockerfile-ts-mode) (js . js-ts-mode) (python . python-ts-mode)
    (css-mode . css-ts-mode) (sh-script . bash-ts-mode) (csharp-mode . csharp-ts-mode)
    (c-ts-mode . (c-ts-mode c++-ts-mode c-or-c++-ts-mode))
    (typescript-ts-mode . (typescript-ts-mode tsx-ts-mode))
    (go-ts-mode . (go-ts-mode go-work-ts-mode go-mod-ts-mode))))

(cl-callf append treesit-language-source-alist
  '((xml "https://github.com/tree-sitter-grammars/tree-sitter-xml")
    (hurl "https://github.com/pfeiferj/tree-sitter-hurl")
    (latex "https://github.com/latex-lsp/tree-sitter-latex")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (clojure "https://github.com/sogaiu/tree-sitter-clojure")
    (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")))

(defun +treesit-install-all-grammars ()
  "Install all grammars in `treesit-language-source-alist'."
  (interactive)
  (cl-letf (((symbol-function #'y-or-n-p) #'always))
    (mapc (lambda (lib) (with-demoted-errors "Error, feature not fount `%S'" (require lib)))
          (mapcar #'car +treesit-builtin-feature-mode-alist))
    (mapc #'treesit-ensure-installed (seq-uniq (mapcar #'car treesit-language-source-alist)))))

(defvar +treesit-mode-lang '((emacs-lisp-mode . elisp) (c++-mode . cpp) (c++-ts-mode . cpp)))

(defun +treesit-get-lang (mode)
  (or (alist-get mode +treesit-mode-lang)
      (intern (string-remove-suffix "-mode" (symbol-name mode)))))

(defun +treesit-create-parser-in-buffer (&optional buffer)
  "Create a `treesit' grammar in BUFFER even if the mode isn't a ts-mode."
  (interactive (list (when current-prefix-arg (get-buffer (read-buffer "Create treesit parser in buffer: ")))))
  (let ((buffer (or buffer (current-buffer)))
        (interact-p (called-interactively-p 'interactive)))
    (if (treesit-available-p)
        (when (or (not (derived-mode-p +treesit-create-parser-modes-deny))
                  (and interact-p (y-or-n-p (format "Creating parsers for `%S' is blacklisted in `+treesit-create-parser-modes-deny', continue?" major-mode))))
          (with-current-buffer buffer
            (if-let* ((lang (+treesit-get-lang major-mode))
                      ((treesit-ensure-installed lang)))
                (when (or (not (treesit-parser-list buffer lang))
                          (and interact-p (y-or-n-p (format "The %S buffer already have a %S language parser, continue?" buffer lang))))
                  (treesit-parser-create lang)
                  (when interact-p (message "Created a %S language parser in %S" lang buffer)))
              (when interact-p (user-error "No installed tree-sitter grammar for mode `%s'" major-mode)))))
      (when interact-p (user-error "Tree-sitter isn't available in this Emacs build")))))


(provide 'me-treesit-x)

;;; me-treesit-x.el ends here
