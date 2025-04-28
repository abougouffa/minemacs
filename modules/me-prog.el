;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-04-27

;;; Commentary:

;;; Code:

;; Automatically manage `treesit' grammars
(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :when (+emacs-options-p 'tree-sitter)
  :hook (minemacs-build-functions . treesit-auto-install-all)
  :hook (minemacs-lazy . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
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
                 :lang 'markdown :ts-mode 'markdown-ts-mode :remap '(poly-markdown-mode markdown-mode) :requires 'markdown-inline
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser" :source-dir "tree-sitter-markdown/src" :ext "\\.md\\'")
                (make-treesit-auto-recipe
                 :lang 'markdown-inline :requires 'markdown
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser" :source-dir "tree-sitter-markdown-inline/src")
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

  (defvar +treesit-auto-create-parser-modes-deny '(org-mode))

  ;; Create `treesit' parsers when they are available even in non-treesit modes.
  ;; This is useful for packages like `virtual-format', `treesit-fold', `expreg'
  ;; and `ts-movement'.
  (defun +treesit-auto-create-parser-in-buffer (&optional buffer)
    "Create `treesit' in BUFF-NAME, even if the mode isn't a ts-mode."
    (interactive (list (when current-prefix-arg (get-buffer (read-buffer "Create treesit parser in buffer: ")))))
    (let ((buffer (or buffer (current-buffer)))
          (interact-p (called-interactively-p 'interactive)))
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


;; Tree-sitter based code folding
(use-package treesit-fold
  :straight (:host github :repo "emacs-tree-sitter/treesit-fold")
  :when (+emacs-options-p 'tree-sitter))


;; Boost `eglot' using `emacs-lsp-booster' (github.com/blahgeek/emacs-lsp-booster)
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster"))


;; Emacs text actions using LSP symbol information
(use-package gambol
  :straight (:host codeberg :repo "woolsweater/gambol.el")
  :hook (eglot-managed-mode . gambol-mode)
  :bind
  (("M-g ," . gambol:go-to-previous)
   ("M-g ." . gambol:go-to-next)
   ([remap mc/mark-all-dwim] . gambol:edit-all)
   :map gambol-repeat-map
   ("," . gambol:go-to-previous)
   ("." . gambol:go-to-next)
   ("e" . gambol:edit-all)
   ("o" . gambol:occur))
  :init
  (with-eval-after-load 'embark (gambol:install-embark-integration))) ; Integrate with `embark'


;; Consult integration with Eglot
(use-package consult-eglot
  :straight t)


;; Run code formatter on buffer contents without moving point
(use-package apheleia
  :straight t
  :custom
  (apheleia-remote-algorithm 'local) ; format remote files using local formatters
  :config
  (add-hook
   'nxml-mode-hook
   (satch-defun +xmllint--set-indent-h ()
     (setenv "XMLLINT_INDENT" (make-string nxml-child-indent (string-to-char " ")))))
  (push '(nxml-mode . xmllint) apheleia-mode-alist)

  ;; Append the "-style" option to the `clang-format' command
  (let ((clang (assq 'clang-format apheleia-formatters)))
    (setcdr clang (append (cdr clang) '("-style" (+clang-format-get-style))))))


;; Define commands which run reformatters on the current Emacs buffer
(use-package reformatter
  :straight t
  :config
  (reformatter-define ref-black
    :program "black"
    :args (append
           (when (equal "pyi" (and buffer-file-name (file-name-extension buffer-file-name)))
             '("--pyi"))
           '("-"))
    :lighter "Black ")

  (reformatter-define ref-lua-format
    :program "lua-format"
    :args (append
           (when-let* ((indent (cond ((derived-mode-p 'lua-ts-mode) lua-ts-indent-offset)
                                     ((derived-mode-p 'lua-mode) lua-indent-level))))
             (list (format "--indent-width=%d")))
           (unless indent-tabs-mode
             '("--no-use-tab")))
    :lighter "LuaFmt ")

  (reformatter-define ref-clang-format
    :program "clang-format"
    :args (list
           "-style" (+clang-format-get-style)
           "-assume-filename"
           (or (buffer-file-name)
               (alist-get major-mode
                          '(((c-mode c-ts-mode)               . ".c")
                            ((csharp-mode csharp-ts-mode)     . ".cs")
                            ((c++-mode c++-ts-mode)           . ".cpp")
                            ((cuda-mode cuda-ts-mode)         . ".cu")
                            ((glsl-mode glsl-ts-mode)         . ".glsl")
                            ((java-mode java-ts-mode)         . ".java")
                            ((objc-mode objc-ts-mode)         . ".m")
                            ((protobuf-mode protobuf-ts-mode) . ".proto"))
                          ".c" nil
                          (lambda (lst elt) (memq elt lst)))))
    :lighter "ClangFmt "))


;; Out of the box code execution from editing buffer
(use-package quickrun
  :straight t
  :bind (([f5] . quickrun)))


;; An Emacs "jump to definition" package for 50+ languages
(use-package dumb-jump
  :straight t
  :after xref
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; Use `dumb-jump' as `xref' backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; Combine multiple Xref backends
(use-package xref-union
  :straight t
  :commands (xref-union-mode)
  :custom
  ;; BUG+HACK: When in `xref-union-mode', the `xref-union--backend' seems to
  ;; access all the backends, including the ones that aren't enabled locally.
  ;; The list includes `etags' which is annoying since it asks about the TAGS
  ;; file, which interrupts `xref-union' from trying the rest of the backends.
  ;; So, lets exclude `etags' since I'm not using it, the predicate function can
  ;; be modified to check for other conditions (for example: enable `etags' in
  ;; some circumstances)
  (xref-union-excluded-backends #'+xref-union--exclude-backends-predicate)
  :config
  (defun +xref-union--exclude-backends-predicate (backend)
    (memq backend '(etags--xref-backend))))


;; Highlight TODO keywords
(use-package hl-todo
  :straight (:host github :repo "tarsius/hl-todo")
  :hook (prog-mode . hl-todo-mode)
  :config
  (cl-callf append hl-todo-keyword-faces
    '(("BUG"   . "#ee5555")
      ("FIX"   . "#0fa050")
      ("PROJ"  . "#447f44")
      ("IDEA"  . "#0fa050")
      ("INFO"  . "#0e9030")
      ("TWEAK" . "#fe9030")
      ("PERF"  . "#e09030"))))


;; Emacs headerline indication of where you are in a large project
(use-package breadcrumb
  :straight t
  :hook ((c-mode c++-mode c-ts-base-mode python-base-mode rust-ts-mode sh-mode bash-ts-mode) . breadcrumb-local-mode)
  :config
  ;; Don't show the project/file name in the header by just a file icon
  (with-eval-after-load 'nerd-icons
    (advice-add
     'breadcrumb-project-crumbs :override
     (satch-defun +breadcrumb--project:override-a ()
       (concat " " (if-let* ((file buffer-file-name))
                       (nerd-icons-icon-for-file file)
                     (nerd-icons-icon-for-mode major-mode)))))))


;; Emacs viewer for DevDocs, offline documentation for programming languages and libraries
(use-package devdocs
  :straight t
  :when (+emacs-options-p 'libxml2))


;; Coccinelle: Complex style-preserving source-to-source transformations
(use-package cocci
  :when (file-exists-p "/usr/share/emacs/site-lisp/cocci.el")
  :load-path "/usr/share/emacs/site-lisp/"
  :mode ("\\.iso$" . cocci-mode)
  :mode ("\\.cocci$" . cocci-mode))


(provide 'me-prog)
;;; me-prog.el ends here
