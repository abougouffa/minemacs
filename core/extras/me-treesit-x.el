;;; me-treesit-x.el --- Extra tweaks Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-11
;; Last modified: 2025-08-21

;;; Commentary:

;;; Code:

;;;###autoload(with-eval-after-load 'treesit (require 'me-treesit-x))

(defvar +treesit-create-parser-modes-deny nil)

(cl-callf append treesit-language-source-alist
  '((xml "https://github.com/tree-sitter-grammars/tree-sitter-xml")
    (hurl "https://github.com/pfeiferj/tree-sitter-hurl")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")))

(defvar +treesit-mode-lang '((emacs-lisp-mode . elisp) (c++-mode . cpp) (c++-ts-mode . cpp)))

(defun +treesit-get-lang (mode)
  (or (alist-get mode +treesit-mode-lang)
      (intern (string-remove-suffix "-mode" (symbol-name mode)))))

(defun +treesit-create-parser-in-buffer (&optional buffer)
  "Create a `treesit' grammar in BUFFER even if the mode isn't a ts-mode."
  (interactive (list (when current-prefix-arg (get-buffer (read-buffer "Create treesit parser in buffer: ")))))
  (let ((buffer (or buffer (current-buffer)))
        (interact-p (interactive-p)))
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
