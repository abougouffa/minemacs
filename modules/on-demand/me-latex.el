;;; me-latex.el --- LaTeX extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-latex
  :companion-packages '(((latex-mode tex-mode doctex-mode bibtex-mode bibtex-style-mode) . (auctex auctex-latexmk latex-preview-pane))))


;; Integrated environment for TeX
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
  (TeX-source-correlate-start-server nil) ; don't start the Emacs server when correlating sources.
  (TeX-electric-math '("$" . "$")) ; auto close inline equations
  (TeX-electric-sub-and-superscript t) ; automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-save-query nil) ; just save, don't ask before each compilation.
  (TeX-engine 'xetex) ; use XeLaTeX by default
  :config
  ;; Adapted from Doom Emacs and Crafted Emacs
  (when (functionp 'pdf-tools-install)
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))

  ;; To have the buffer refresh after compilation
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Compile to PDF by default
  (TeX-PDF-mode 1))


;; Integrated environment for LaTeX
(use-package latex
  :straight auctex
  :hook (LaTeX-mode . latex-math-mode)
  :hook (LaTeX-mode . turn-on-reftex)
  :custom
  ;; Add the TOC entry to the sectioning hooks.
  (LaTeX-fill-break-at-separators nil)
  (LaTeX-item-indent 0)
  (LaTeX-electric-left-right-brace t)
  (LaTeX-reftex-cite-format-auto-activate nil)
  :config
  ;; Adapted from Doom Emacs and Crafted Emacs
  ;; Set a correct indentation in a few additional environments
  (cl-callf append LaTeX-indent-environment-list
    '(("tikzcd" LaTeX-indent-tabular)
      ("lstlisting" current-indentation)
      ("tikzpicture" current-indentation)))

  ;; Add a few macros and environment as verbatim
  (cl-callf append LaTeX-verbatim-environments '("lstlisting" "Verbatim"))
  (cl-callf append LaTeX-verbatim-environments '("lstlisting" "Verbatim"))
  (add-to-list 'LaTeX-verbatim-macros-with-braces "lstinline")
  (add-to-list 'LaTeX-verbatim-macros-with-delims "lstinline"))


;; Add LatexMk support to AUCTeX
(use-package auctex-latexmk
  :straight t
  :after latex
  :demand
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  ;; Adapted from Doom Emacs
  ;; Enable only if Latexmk is available
  (when (executable-find "latexmk")
    (setq-default
     TeX-command-default "LatexMk-2"
     TeX-command-list
     (cons
      '("LatexMk-2" "latexmk -shell-escape %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t" TeX-run-latexmk nil
        (plain-tex-mode latex-mode doctex-mode) :help "Run LatexMk with shell-escape")
      TeX-command-list))

    ;; Add LatexMk as a TeX target.
    (auctex-latexmk-setup)))


;; Makes LaTeX editing less painful by providing a updatable preview pane
(use-package latex-preview-pane
  :straight t)


(provide 'on-demand/me-latex)
;;; me-latex.el ends here
