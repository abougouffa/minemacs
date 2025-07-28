;;; me-prog.el --- Programming stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:

;; Boost `eglot' using `emacs-lsp-booster' (github.com/blahgeek/emacs-lsp-booster)
(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :custom
  ;; Emacs' 30 JSON parser is much faster, no need to byte-encode, see:
  ;; https://github.com/jdtsmith/eglot-booster?tab=readme-ov-file#io-only
  (eglot-booster-io-only (>= emacs-major-version 30)))


;; Consult integration with Eglot
(use-package consult-eglot
  :straight t
  :unless (+package-disabled-p 'consult 'me-completion)
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t))))


;; Run code formatter on buffer contents without moving point
(use-package apheleia
  :straight t
  :custom
  (apheleia-remote-algorithm 'local) ; format remote files using local formatters
  :hook (prog-mode . +apheleia-mode-maybe)
  :config
  (advice-add
   'apheleia--run-formatter-process :around
   (satch-defun +apheleia--set-process-environment:around-a (orig-fn command buffer remote callback stdin formatter)
     (let ((process-environment
            (if (eq formatter 'xmllint)
                (cons (format "XMLLINT_INDENT=%s" (if indent-tabs-mode "\t" (make-string nxml-child-indent (string-to-char " "))))
                      process-environment)
              process-environment)))
       (funcall orig-fn command buffer remote callback stdin formatter))))

  (cl-callf append apheleia-mode-alist
    '((nxml-mode . xmllint)
      (protobuf-mode . clang-format)
      (protobuf-ts-mode . clang-format)))

  (add-hook
   'apheleia-skip-functions
   (satch-defun +smerge--unresolved-conflict-p ()
     (save-excursion (goto-char (point-min))
                     ;; Regexps from `smerge-begin-re' and `smerge-end-re'
                     (or (re-search-forward "^<<<<<<< \\(.*\\)\n" nil t)
                         (re-search-forward "^>>>>>>> \\(.*\\)\n" nil t)))))

  (defun +apheleia-mode-maybe ()
    (when (and (+clang-format-get-lang) (+clang-format-config-file))
      (apheleia-mode 1)))

  ;; For `clang-format', use the command from `+clang-format-command', and
  ;; append the "-style" option
  (let ((clang (assq 'clang-format apheleia-formatters)))
    (setcdr clang (cons +clang-format-command (append (cddr clang) '((+clang-format-get-style)))))))


;; An Emacs "jump to definition" package for 50+ languages
(use-package dumb-jump
  :straight t
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate) ; Use `dumb-jump' as `xref' backend
  :config
  (with-eval-after-load 'project
    (cl-callf append dumb-jump-project-denoters project-vc-extra-root-markers)))


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


;; Interactive macro-expander for Emacs Lisp and C
(use-package macrostep
  :straight t
  :hook ((c-mode c++-mode c-ts-mode c++-ts-mode) . macrostep-c-mode-hook)
  :bind (:package elisp-mode :map emacs-lisp-mode-map ("C-c m" . macrostep-expand))
  :bind (:package cc-mode :map c-mode-map ("C-c m" . macrostep-expand))
  :bind (:package cc-mode :map c++-mode-map ("C-c m" . macrostep-expand))
  :bind (:package c-ts-mode :map c-ts-mode-map ("C-c m" . macrostep-expand))
  :bind (:package c-ts-mode :map c++-ts-mode-map ("C-c m" . macrostep-expand)))


;; Emacs headerline indication of where you are in a large project
(use-package breadcrumb
  :straight t
  :hook ((c-mode c++-mode c-ts-base-mode python-base-mode rust-ts-mode sh-mode bash-ts-mode) . breadcrumb-local-mode)
  :config
  ;; Don't show the project/file name in the header, show only an icon
  (with-eval-after-load 'nerd-icons
    (advice-add
     'breadcrumb-project-crumbs :override
     (satch-defun +breadcrumb--project:override-a ()
       (concat " " (if-let* ((file buffer-file-name))
                       (nerd-icons-icon-for-file file)
                     (nerd-icons-icon-for-mode major-mode)))))))


;; Simple and fast C mode for amalgamated (big) files
(use-package simpc-mode
  :straight (:host github :repo "rexim/simpc-mode")
  :commands (simpc-mode))


;; Generate C++ method implementations from declarations using `treesit'
(use-package cpp-func-impl
  :straight (:host github :repo "dheerajshenoy/cpp-func-impl.el"))


(provide 'me-prog)
;;; me-prog.el ends here
