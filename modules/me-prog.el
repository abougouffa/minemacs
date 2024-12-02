;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Automatically manage `treesit' grammars
(use-package treesit-auto
  :vc (:url "https://github.com/renzmann/treesit-auto")
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


;; Move and edit code blocks based on tree-sitter AST
(use-package ts-movement
  :vc (:url "https://github.com/abougouffa/ts-movement")
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


;; Tree-sitter based code folding
(use-package treesit-fold
  :vc (:url "https://github.com/emacs-tree-sitter/treesit-fold")
  :when (+emacs-options-p 'tree-sitter))


;; Boost `eglot' using `emacs-lsp-booster' (github.com/blahgeek/emacs-lsp-booster)
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :init
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode 1)))


;; Emacs text actions using LSP symbol information
(use-package gambol
  :vc (:url "https://codeberg.org/woolsweater/gambol.el" :ignored-files ("gambol-embark.el"))
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
  (with-eval-after-load 'embark (gambol:install-embark-integration)) ; Integrate with `embark'
  (defun +gambol:occur-dwim ()
    "Call `gambol:occur' if in an Eglot managed buffer, fallback to `occur'."
    (interactive)
    (unless (and (featurep 'eglot) (eglot-managed-p) (ignore-errors (gambol:occur) t))
      (call-interactively #'occur))))

(use-package gambol-embark
  :vc (:url "https://codeberg.org/woolsweater/gambol.el" :ignored-files ("gambol.el"))
  :after embark eglot)

;; Structured editing and navigation in Emacs with Tree-Sitter
(use-package combobulate-setup
  ;; TEMP: The "combobulate.el" contains a lot of autoloads that prevent lazy loading, we exclude the main file
  :vc (combobulate :url "https://github.com/mickeynp/combobulate" :ignored-files ("combobulate.el" "tests/*"))
  :when (and (not (+emacs-options-p 'os/win)) (+emacs-options-p 'tree-sitter)) ; TEMP: disable on Windows
  :custom
  (combobulate-key-prefix "C-c b") ; "C-c o" is used by `minemacs-open-thing-map'
  :config
  ;; TEMP+FIX: Basically, load the same features that would be loaded by "combobulate.el"
  (dolist (feature '(combobulate-rules
                     combobulate-procedure combobulate-navigation
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
  :ensure t)


;; Run code formatter on buffer contents without moving point
(use-package apheleia
  :ensure t
  :custom
  (apheleia-remote-algorithm 'local) ; format remote files using local formatters
  :config
  (add-hook
   'nxml-mode-hook
   (satch-defun +xmllint--set-indent-h ()
     (setenv "XMLLINT_INDENT" (make-string nxml-child-indent (string-to-char " ")))))
  (push '(nxml-mode . xmllint) apheleia-mode-alist)

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

  ;; Append the "-style" option to the `clang-format' command
  (let ((clang (assq 'clang-format apheleia-formatters)))
    (setcdr clang (append (cdr clang) '("-style" (+clang-format-get-style))))))


;; Auto-format source code in many languages with one command
(use-package format-all
  :ensure t)


;; Out of the box code execution from editing buffer
(use-package quickrun
  :ensure t
  :bind (([f5] . quickrun)))


;; An Emacs "jump to definition" package for 50+ languages
(use-package dumb-jump
  :ensure t
  :after xref
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; Use `dumb-jump' as `xref' backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


;; Combine multiple Xref backends
(use-package xref-union
  :ensure t
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
  :vc (:url "https://github.com/tarsius/hl-todo")
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
  :ensure t
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
  :ensure t
  :when (+emacs-options-p 'libxml2))


(provide 'me-prog)

;;; me-prog.el ends here
