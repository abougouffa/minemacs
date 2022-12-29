;;; me-latex.el --- LaTeX related stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; Adapted from Doom Emacs
(use-package auctex
  :straight t
  :hook ((tex-mode TeX-mode latex-mode LaTeX-mode) . TeX-source-correlate-mode)
  :general
  (+map-local :keymaps '(tex-mode-map TeX-mode-map latex-mode-map LaTeX-mode-map)
    "c" #'TeX-command-run-all
    "m" #'TeX-command-master
    "v" #'TeX-view)
  :custom
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t)  ; parse on save
  (TeX-auto-local ".auctex-auto") ; use hidden directories for AUCTeX files.
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server nil) ; don't start the Emacs server when correlating sources.
  (TeX-electric-sub-and-superscript t) ; automatically insert braces after sub/superscript in `LaTeX-math-mode'.
  (TeX-save-query nil) ; just save, don't ask before each compilation.
  :config
  (with-eval-after-load 'pdf-tools
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))))

;; Adapted from Doom Emacs
(use-package auctex-latexmk
  :straight t
  :after latex
  :hook (LaTeX-mode . +tex--set-latexmk-as-default-cmd-h)
  :defines +tex--set-latexmk-as-default-cmd-h
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (defun +tex--set-latexmk-as-default-cmd-h ()
    (setq TeX-command-default "LatexMk"))
  ;; Add LatexMk as a TeX target.
  (auctex-latexmk-setup))

(use-package bibtex
  :straight (:type built-in)
  :defer t
  :hook (bibtex-mode . display-line-numbers-mode)
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20)
  :general
  (+map-local :keymaps 'bibtex-mode-map
    "l" #'bibtex-fill-entry
    "r" #'bibtex-reformat))

;; Adapted from Doom Emacs
(use-package reftex
  :straight (:type built-in)
  :hook (LaTeX-mode . turn-on-reftex)
  :hook (reftex-toc-mode . +reftex--toc-tweaks-h)
  :defines +reftex--toc-tweaks-h
  :general
  (+map-local :keymaps 'reftex-mode-map
    ";" 'reftex-toc)
  (+map-key :keymaps 'reflex-toc-mode-map
    "j"   #'next-line
    "k"   #'previous-line
    "q"   #'kill-buffer-and-window
    "ESC" #'kill-buffer-and-window)
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
  (with-eval-after-load 'evil
    (add-hook 'reftex-mode-hook #'evil-normalize-keymaps))
  (defun +reftex--toc-tweaks-h ()
    (reftex-toc-rescan)


(provide 'me-latex)
