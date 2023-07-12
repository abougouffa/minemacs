;;; me-latex.el --- LaTeX related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

;; Adapted from Doom Emacs
(use-package tex
  :straight auctex
  :hook ((tex-mode TeX-mode latex-mode LaTeX-mode) . TeX-source-correlate-mode)
  :hook ((tex-mode TeX-mode latex-mode LaTeX-mode) . hs-minor-mode)
  :custom
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t)  ; parse on save
  (TeX-auto-local ".auctex-auto") ; use hidden directories for AUCTeX files.
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-method 'synctex)
  (TeX-electric-math '("$" . "$")) ; auto close inline equations
  (TeX-source-correlate-start-server nil) ; don't start the Emacs server when correlating sources.
  (TeX-electric-sub-and-superscript t) ; automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-save-query nil) ; just save, don't ask before each compilation.
  (TeX-engine 'xetex) ; use XeLaTeX by default
  :init
  (+map-local! :keymaps '(tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
    "c" #'TeX-command-run-all
    "m" #'TeX-command-master
    "e" #'TeX-engine-set
    "v" #'TeX-view)
  :config
  (when (functionp 'pdf-tools-install)
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))

  ;; To have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Compile to PDF by default
  (TeX-PDF-mode 1))

(use-package latex
  :straight auctex
  :hook (latex-mode . latex-math-mode)
  :custom
  ;; Add the TOC entry to the sectioning hooks.
  (LaTeX-fill-break-at-separators nil)
  (LaTeX-item-indent 0)
  (LaTeX-electric-left-right-brace t)
  :config
  ;; Set a correct indentation in a few additional environments
  (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
  (add-to-list 'LaTeX-indent-environment-list '("tikzcd" LaTeX-indent-tabular))
  (add-to-list 'LaTeX-indent-environment-list '("tikzpicture" current-indentation))

  ;; Add a few macros and environment as verbatim
  (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  (add-to-list 'LaTeX-verbatim-environments "Verbatim")
  (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
  (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline"))

;; Adapted from Doom Emacs
(use-package auctex-latexmk
  :straight t
  :after latex
  :demand t
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Enable only if Latexmk is available
  (when (executable-find "latexmk")
    (setq-default
     TeX-command-list
     (cons
      '("LatexMk-2" "latexmk -shell-escape %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t" TeX-run-latexmk nil
        (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk with shell-escape")
      TeX-command-list))

    (add-hook
     'LaTeX-mode-hook
     (defun +tex--set-latexmk-as-default-cmd-h ()
       (setq TeX-command-default "LatexMk-2")))

    ;; Add LatexMk as a TeX target.
    (auctex-latexmk-setup)))

(use-package me-latex-fonts
  :after latex
  :demand t)


(provide 'me-latex)

;;; me-latex.el ends here
