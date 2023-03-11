;;; me-tree-sitter.el --- Tree-sitter integraion for Emacs28 and earlier -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package tree-sitter
  :straight t
  :defer 5
  :when (eq sys/arch 'x86_64)
  :preface
  (+fn-inhibit-messages! tsc-dyn-get-ensure)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :straight t
  :preface
  (+fn-inhibit-messages! tree-sitter-langs-install-grammars)
  :after tree-sitter
  :demand t)

(use-package evil-textobj-tree-sitter
  :straight t
  :after evil tree-sitter
  :demand t
  :config
  ;; evil-textobj-tree-sitter comes with no default keybindings,
  ;; Here is a keybindings (vaX) stolen from here:
  ;; github.com/meain/dotfiles/blob/master/emacs/.config/emacs/init.el
  (define-key
   evil-outer-text-objects-map
   "m" (cons "evil-import"
             (evil-textobj-tree-sitter-get-textobj
               "import"
               '((python-mode . [(import_statement) @import])
                 (go-mode . [(import_spec) @import])
                 (rust-mode . [(use_declaration) @import])
                 (c-mode . [(preproc_include) @import])
                 (c++-mode . [(preproc_include) @import])))))
  (define-key
   evil-outer-text-objects-map
   "f" (cons "evil-outer-function" (evil-textobj-tree-sitter-get-textobj "function.outer")))
  (define-key
   evil-inner-text-objects-map
   "f" (cons "evil-inner-function" (evil-textobj-tree-sitter-get-textobj "function.inner")))
  (define-key
   evil-outer-text-objects-map
   "c" (cons "evil-outer-class" (evil-textobj-tree-sitter-get-textobj "class.outer")))
  (define-key
   evil-inner-text-objects-map
   "c" (cons "evil-inner-class" (evil-textobj-tree-sitter-get-textobj "class.inner")))
  (define-key
   evil-outer-text-objects-map
   "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key
   evil-inner-text-objects-map
   "n" (cons "evil-outer-comment" (evil-textobj-tree-sitter-get-textobj "comment.outer")))
  (define-key
   evil-outer-text-objects-map
   "o" (cons "evil-outer-loop" (evil-textobj-tree-sitter-get-textobj "loop.outer")))
  (define-key
   evil-inner-text-objects-map
   "o" (cons "evil-inner-loop" (evil-textobj-tree-sitter-get-textobj "loop.inner")))
  (define-key
   evil-outer-text-objects-map
   "v" (cons "evil-outer-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.outer")))
  (define-key
   evil-inner-text-objects-map
   "v" (cons "evil-inner-conditional" (evil-textobj-tree-sitter-get-textobj "conditional.inner")))
  (define-key
   evil-inner-text-objects-map
   "a" (cons "evil-inner-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.inner")))
  (define-key
   evil-outer-text-objects-map
   "a" (cons "evil-outer-parameter" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))

  (define-key
   evil-normal-state-map
   (kbd "]a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner"))))
  (define-key
   evil-normal-state-map
   (kbd "[a") (cons "goto-parameter-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t))))
  (define-key
   evil-normal-state-map
   (kbd "]A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[A") (cons "goto-parameter-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "parameter.inner" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer"))))
  (define-key
   evil-normal-state-map
   (kbd "[v") (cons "goto-conditional-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t))))
  (define-key
   evil-normal-state-map
   (kbd "]V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[V") (cons "goto-conditional-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer"))))
  (define-key
   evil-normal-state-map
   (kbd "[c") (cons "goto-class-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t))))
  (define-key
   evil-normal-state-map
   (kbd "]C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[C") (cons "goto-class-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer"))))
  (define-key
   evil-normal-state-map
   (kbd "[n") (cons "goto-comment-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t))))
  (define-key
   evil-normal-state-map
   (kbd "]N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" nil t))))
  (define-key
   evil-normal-state-map
   (kbd "[N") (cons "goto-comment-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "comment.outer" t t))))
  (define-key
   evil-normal-state-map
   (kbd "]f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer") (reposition-window))))
  (define-key
   evil-normal-state-map
   (kbd "[f") (cons "goto-function-start" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t) (reposition-window))))
  (define-key
   evil-normal-state-map
   (kbd "]F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t) (reposition-window))))
  (define-key
   evil-normal-state-map
   (kbd "[F") (cons "goto-function-end" (+cmdfy! (evil-textobj-tree-sitter-goto-textobj "function.outer" t t) (reposition-window)))))
