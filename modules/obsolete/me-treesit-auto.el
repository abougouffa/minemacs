;;; me-treesit-auto.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2025-07-16
;; Last modified: 2025-07-16

;;; Commentary:

;;; Code:


;; Automatically manage `treesit' grammars
(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :when (featurep 'feat/tree-sitter)
  :hook
  (minemacs-build-functions . treesit-auto-install-all)
  (minemacs-lazy . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :init
  ;; Hide the `treesit-auto-install-all' buffer
  (add-to-list 'display-buffer-alist '("\\*Treesit-auto install candidates\\*" (display-buffer-no-window) (allow-no-window . t)))
  :config
  ;; Add extra grammars
  ;; BUG+FIX: Remove the Markdown grammar to install it correctly (renzmann/treesit-auto#102)
  (let* ((extra-recipes
          (list (make-treesit-auto-recipe
                 :lang 'xml :ts-mode 'xml-ts-mode :remap '(nxml-mode xml-mode)
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-xml"
                 :source-dir "xml/src" :ext "\\.xml\\'")
                (make-treesit-auto-recipe
                 :lang 'hurl :ts-mode 'hurl-ts-mode :remap 'hurl-mode
                 :url "https://github.com/pfeiferj/tree-sitter-hurl"
                 :ext "\\.hurl\\'")
                (make-treesit-auto-recipe
                 :lang 'llvm :ts-mode 'llvm-ts-mode :remap 'llvm-mode
                 :url "https://github.com/benwilliamgraham/tree-sitter-llvm"
                 :ext "\\.ll\\'")
                (make-treesit-auto-recipe
                 :lang 'elisp :ts-mode 'emacs-lisp-ts-mode :remap 'emacs-lisp-mode
                 :url "https://github.com/Wilfred/tree-sitter-elisp"
                 :ext "\\.eld?\\'")
                (make-treesit-auto-recipe
                 :lang 'bitbake :ts-mode 'bitbake-ts-mode :remap 'bitbake-mode
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-bitbake"
                 :ext "\\.bb\\(append\\)?\\'")
                (make-treesit-auto-recipe
                 :lang 'zig :ts-mode 'zig-ts-mode :remap 'zig-mode
                 :url "https://github.com/GrayJack/tree-sitter-zig"
                 :ext "\\.\\(zig\\|zon\\)\\'"))))
    ;; First, delete the duplicate recipes already present in the list, if any
    (cl-callf2 cl-delete-if
        (lambda (lang) (memq (treesit-auto-recipe-lang lang) (mapcar #'treesit-auto-recipe-lang extra-recipes)))
        treesit-auto-recipe-list)
    ;; Then, add the extra recipes to the list
    (cl-callf append treesit-auto-recipe-list extra-recipes)
    (setq treesit-auto-langs (mapcar #'treesit-auto-recipe-lang treesit-auto-recipe-list)))

  ;; Ensure that installed tree-sitter languages have their corresponding `x-ts-mode' added to `auto-mode-alist'
  (treesit-auto-add-to-auto-mode-alist 'all)

  (defvar +treesit-auto-create-parser-modes-deny nil)

  ;; Create `treesit' parsers when they are available even in non-treesit modes.
  ;; This is useful for packages like `virtual-format', `treesit-fold', `expreg'
  ;; and `ts-movement'.
  (defun +treesit-auto-create-parser-in-buffer (&optional buffer)
    "Create `treesit' in BUFF-NAME, even if the mode isn't a ts-mode."
    (interactive (list (when current-prefix-arg (get-buffer (read-buffer "Create treesit parser in buffer: ")))))
    (let ((buffer (or buffer (current-buffer)))
          (interact-p (interactive-p)))
      (if (treesit-available-p)
          (when (or (not (derived-mode-p +treesit-auto-create-parser-modes-deny))
                    (and interact-p (y-or-n-p "Creating parsers for `%S' is blacklisted in `+treesit-auto-create-parser-modes-deny', continue?")))
            (with-current-buffer buffer
              (if-let* ((lang-recipe (cl-find-if (lambda (recipe) (eq major-mode (treesit-auto-recipe-remap recipe)))
                                                 treesit-auto-recipe-list))
                        (lang (treesit-auto-recipe-lang lang-recipe))
                        (lang (and (treesit-language-available-p lang) lang)))
                  (when (or (not (treesit-parser-list buffer lang))
                            (and interact-p (y-or-n-p (format "The %S buffer already have a %S language parser, continue?" buffer lang))))
                    (treesit-parser-create lang)
                    (when interact-p (message "Created a %S language parser in %S" lang buffer)))
                (when interact-p (user-error "No installed tree-sitter grammar for mode `%s'" major-mode)))))
        (when interact-p (user-error "Tree-sitter isn't available in this Emacs build")))))

  (add-hook 'after-change-major-mode-hook '+treesit-auto-create-parser-in-buffer))


(provide 'obsolete/me-treesit-auto)
;;; me-treesit-auto.el ends here
