;;; latex.el --- LaTeX related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>

;; Adapted from Doom Emacs
(use-package tex
  :straight auctex ; (auctex :files ("*.el" "*.info" "dir" "doc" "etc" "images" "latex" "style"))
  :general
  (+map-local :keymaps '(tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
    "c" #'TeX-command-run-all
    "m" #'TeX-command-master
    "v" #'TeX-view)
  :config
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        ;; Use hidden directories for AUCTeX files.
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; Don't start the Emacs server when correlating sources.
        TeX-source-correlate-start-server nil
        ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
        TeX-electric-sub-and-superscript t
        ;; Just save, don't ask before each compilation.
        TeX-save-query nil)
  (when (featurep 'pdf-tools)
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools")))
  (setq-default TeX-master nil))


;; Adapted from Doom Emacs
(use-package auctex-latexmk
  :straight t
  :after latex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;; Set LatexMk as the default.
  (add-hook
   'LaTeX-mode
   (defun +tex--set-latexmk-as-default-cmd-h ()
     (setq TeX-command-default "LatexMk")))
  :config
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))


;; Set up mode for bib files.
(with-eval-after-load 'bibtex
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20)
  (define-key bibtex-mode-map (kbd "C-c \\") #'bibtex-fill-entry))


;; Adapted from Doom Emacs
(use-package reftex
  :straight (:type built-in)
  :hook (LaTeX-mode . reftex-mode)
  :config
  ;; Set up completion for citations and references.
  ;; (set-company-backend! 'reftex-mode 'company-reftex-labels 'company-reftex-citations)
  ;; Get RefTeX working with BibLaTeX, see
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992.
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3
        ;; This is needed when `reftex-cite-format' is set. See
        ;; https://superuser.com/a/1386206
        LaTeX-reftex-cite-format-auto-activate nil)
  (when (featurep 'evil)
    (add-hook 'reftex-mode-hook #'evil-normalize-keymaps))
  (+map-local :keymaps 'reftex-mode-map
    ";" 'reftex-toc)
  (add-hook
   'reftex-toc-mode-hook
   (defun +reftex--toc-tweaks-h ()
     (reftex-toc-rescan)
     (+map-key :keymaps 'reflex-toc-mode-map
       "j"   #'next-line
       "k"   #'previous-line
       "q"   #'kill-buffer-and-window
       "ESC" #'kill-buffer-and-window))))
