;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(unless (+emacs-options-p 'tree-sitter)
  ;; Use the external `tree-sitter' module
  (+load minemacs-obsolete-modules-dir "me-tree-sitter.el"))


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
                 :lang 'xml
                 :ts-mode 'xml-ts-mode
                 :remap '(nxml-mode xml-mode)
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-xml"
                 :source-dir "xml/src"
                 :ext "\\.xml\\'")
                (make-treesit-auto-recipe
                 :lang 'hurl
                 :ts-mode 'hurl-ts-mode
                 :remap 'hurl-mode
                 :url "https://github.com/pfeiferj/tree-sitter-hurl"
                 :ext "\\.hurl\\'")
                (make-treesit-auto-recipe
                 :lang 'markdown
                 :ts-mode 'markdown-ts-mode
                 :remap '(poly-markdown-mode markdown-mode)
                 :requires 'markdown-inline
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser"
                 :source-dir "tree-sitter-markdown/src"
                 :ext "\\.md\\'")
                (make-treesit-auto-recipe
                 :lang 'markdown-inline
                 :ts-mode 'markdown-inline-mode ; Fake mode to make `treesit-auto' happy
                 :remap 'markdown-inline-ts-mode
                 :requires 'markdown
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                 :revision "split_parser"
                 :source-dir "tree-sitter-markdown-inline/src")
                (make-treesit-auto-recipe
                 :lang 'llvm
                 :ts-mode 'llvm-ts-mode
                 :remap 'llvm-mode
                 :url "https://github.com/benwilliamgraham/tree-sitter-llvm"
                 :ext "\\.ll\\'")
                (make-treesit-auto-recipe
                 :lang 'elisp
                 :ts-mode 'emacs-lisp-ts-mode
                 :remap 'emacs-lisp-mode
                 :url "https://github.com/Wilfred/tree-sitter-elisp"
                 :ext "\\.eld?\\'")
                (make-treesit-auto-recipe
                 :lang 'bitbake
                 :ts-mode 'bitbake-ts-mode
                 :remap 'bitbake-mode
                 :url "https://github.com/tree-sitter-grammars/tree-sitter-bitbake"
                 :ext "\\.bb\\(append\\)?\\'")
                (make-treesit-auto-recipe
                 :lang 'zig
                 :ts-mode 'zig-ts-mode
                 :remap 'zig-mode
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

  (defvar +treesit-auto-enable-in-normal-modes-deny '(org-mode))

  ;; Create `treesit' parsers when they are available even in non-treesit modes.
  ;; This is useful for packages like `virtual-format', `treesit-fold', `expreg'
  ;; and `ts-movement'.
  (defun +treesit-enable-available-grammars-on-normal-modes ()
    "Enable `treesit' parses in non-treesit modes."
    (when (treesit-available-p)
      (dolist (recipe treesit-auto-recipe-list)
        (let ((lang (treesit-auto-recipe-lang recipe)))
          (unless (fboundp (treesit-auto-recipe-ts-mode recipe)) ; When the `xxx-ts-mode' is not available
            (dolist (remap-mode (ensure-list (treesit-auto-recipe-remap recipe))) ; Get the basic mode name (non-ts)
              (unless (memq remap-mode +treesit-auto-enable-in-normal-modes-deny) ; Unless this mode is explicitly disabled
                (let ((fn-name (intern (format "+treesit--enable-on-%s-h" remap-mode)))
                      (hook-name (intern (format "%s-hook" remap-mode))))
                  (defalias fn-name (lambda () ; Create the parser if the grammar for the language is available
                                      (when (treesit-language-available-p lang)
                                        (treesit-parser-create lang))))
                  (add-hook hook-name fn-name)))))))))

  (+treesit-enable-available-grammars-on-normal-modes)

  (defun +treesit-create-parser-in-buffer (buff-name)
    "Create `treesit' in BUFF-NAME, even if the mode isn't a ts-mode."
    (interactive (list (if current-prefix-arg (read-buffer "Create treesit parser in buffer: ") (buffer-name))))
    (unless (treesit-available-p) (user-error "Tree-sitter isn't available in this Emacs build"))
    (with-current-buffer (get-buffer buff-name)
      (if-let* ((lang-recipe (cl-find-if
                              (lambda (recipe)
                                (eq major-mode (or (treesit-auto-recipe-remap recipe)
                                                   (treesit-auto-recipe-ts-mode recipe))))
                              treesit-auto-recipe-list))
                (lang (treesit-auto-recipe-lang lang-recipe))
                (lang (and (treesit-language-available-p lang) lang)))
          (treesit-parser-create lang)
        (when (called-interactively-p)
          (user-error "No installed tree-sitter grammar for mode `%s'" major-mode))))))


;; Move and edit code blocks based on tree-sitter AST
(use-package ts-movement
  :straight (:host github :repo "haritkapadia/ts-movement")
  :when (+emacs-options-p 'tree-sitter)
  :hook ((prog-mode conf-mode) . +ts-movement-maybe)
  :init
  (defun +ts-movement-maybe ()
    "Enable `ts-movement-mode' when if `major-mode' is a trees-sitter mode."
    (run-with-timer 1.0 nil (lambda () (when (treesit-parser-list) (ts-movement-mode 1)))))
  :config
  (transient-define-prefix +ts-movement-transient ()
    "Transient for ts-movement."
    [[("d" "delete-overlay-at-point" tsm/delete-overlay-at-point :transient t)
      ("D" "clear-overlays-of-type" tsm/clear-overlays-of-type :transient t)
      ("C-b" "backward-overlay" tsm/backward-overlay :transient t)
      ("C-f" "forward-overlay" tsm/forward-overlay :transient t)
      ("c" "tsm/mc/mark-all-overlays" tsm/mc/mark-all-overlays :transient t)]
     [("a" "node-start" tsm/node-start :transient t)
      ("e" "node-end" tsm/node-end :transient t)
      ("b" "node-prev" tsm/node-prev :transient t)
      ("f" "node-next" tsm/node-next :transient t)]
     [("p" "node-parent" tsm/node-parent :transient t)
      ("n" "node-child" tsm/node-child :transient t)
      ("N" "node-children" tsm/node-children :transient t)
      ("s" "node-children-of-type" tsm/node-children-of-type :transient t)
      ("m" "node-mark" tsm/node-mark :transient t)]]
    [("Q" "Quit" ignore :transient t)]))


;; Extra non-standard functionalities for Eglot
(use-package eglot-x
  :straight (:host github :repo "nemethf/eglot-x")
  :commands (eglot-x-setup))


;; Emacs text actions using LSP symbol information
(use-package gambol
  :straight (:host codeberg :repo "woolsweater/gambol.el")
  :hook (eglot-managed-mode . gambol-mode)
  :bind
  (("M-g ," . gambol:go-to-previous)
   ("M-g ." . gambol:go-to-next)
   ([remap mc/mark-all-dwim] . gambol:edit-all)
   ([remap occur] . +gambol:occur-dwim)
   :map gambol-repeat-map
   ("," . gambol:go-to-previous)
   ("." . gambol:go-to-next)
   ("e" . gambol:edit-all)
   ("o" . gambol:occur))
  :init
  (defun +gambol:occur-dwim ()
    "Call `gambol:occur' if in an Eglot managed buffer, fallback to `occur'."
    (interactive)
    (unless (and (featurep 'eglot) (eglot-managed-p) (ignore-errors (gambol:occur) t))
      (call-interactively #'occur))))


;; Structured editing and navigation in Emacs with Tree-Sitter
(use-package combobulate-setup
  :straight (combobulate
             :host github
             :repo "mickeynp/combobulate"
             :nonrecursive t ; Cloning the `html-ts-mode' submodule causes problems
             :files (:defaults (:exclude "combobulate.el"))) ; TEMP: The "combobulate.el" contains a lot of autoloads that prevent lazy loading
  :when (and (not (+emacs-options-p 'os/win)) (+emacs-options-p 'tree-sitter)) ; TEMP: disable on Windows
  :custom
  (combobulate-key-prefix "C-c b") ; "C-c o" is used by `minemacs-open-thing-map'
  :config
  ;; TEMP+FIX: Basically, load the same features that would be loaded by "combobulate.el"
  (dolist (feature '(combobulate-rules combobulate-procedure combobulate-navigation
                     combobulate-manipulation combobulate-envelope combobulate-display
                     combobulate-ui combobulate-misc combobulate-query combobulate-cursor
                     combobulate-toml combobulate-html combobulate-python combobulate-js-ts
                     combobulate-css combobulate-yaml combobulate-json combobulate-go))
    (require feature))

  ;; The "M-<up/down/left/right>" keys are used globally by `drag-stuff', lets
  ;; unset them for `combobulate' and use "M-S-<up/down/left/right>" instead.
  (mapc (lambda (k) (keymap-unset combobulate-key-map k 'remove)) '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
  (keymap-set combobulate-key-map "M-S-<up>" #'combobulate-splice-up)
  (keymap-set combobulate-key-map "M-S-<down>" #'combobulate-splice-down)
  (keymap-set combobulate-key-map "M-S-<left>" #'combobulate-splice-self)
  (keymap-set combobulate-key-map "M-S-<right>" #'combobulate-splice-parent))


;; Consult integration with Eglot
(use-package consult-eglot
  :straight t)


;; Helper function to get the style for "clang-format"
(defun +clang-format-get-style ()
  "Get the \"-style\" argument for clang-format."
  (if-let* ((conf-file ".clang-format")
            (dir (locate-dominating-file
                  (or (+project-safe-root) default-directory)
                  conf-file)))
      (expand-file-name conf-file dir)
    (let ((indent
           (cond
            ((derived-mode-p 'c-ts-mode 'c++-ts-mode) 'c-ts-mode-indent-offset)
            ((derived-mode-p 'java-ts-mode) 'java-ts-mode-indent-offset)
            ((derived-mode-p 'csharp-ts-mode) 'csharp-ts-mode-indent-offset)
            ((derived-mode-p 'protobuf-ts-mode) 'protobuf-ts-mode-indent-offset)
            ((derived-mode-p 'c-mode 'c++-mode 'csharp-mode 'opencl-c-mode 'protobuf-mode 'cuda-mode) 'c-basic-offset))))
      (format "{IndentWidth: %d, TabWidth: %d}"
              (or (and indent (symbol-value indent)) standard-indent)
              (or (and indent (symbol-value indent)) tab-width)))))


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


;; Auto-format source code in many languages with one command
(use-package format-all
  :straight t)


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


;; Colorize color names in buffers
(use-package rainbow-mode
  :straight t)


;; Boost `eglot' using `emacs-lsp-booster' (github.com/blahgeek/emacs-lsp-booster)
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster"))


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


;; Show cognitive complexity of code in Emacs 29+ (treesit-based)
(use-package cognitive-complexity
  :straight (:host github :repo "emacs-vs/cognitive-complexity"))


(provide 'me-prog)

;;; me-prog.el ends here
