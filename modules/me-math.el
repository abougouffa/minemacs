;;; me-math.el --- Mathematics stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(defconst +maxima-path "/usr/share/emacs/site-lisp/maxima/")
(defconst +maxima-available-p (and (executable-find "maxima") (file-directory-p +maxima-path)))


;; Major modes for writing Maxima code
(use-package maxima
  :load-path +maxima-path
  :when +maxima-available-p
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :commands (inferior-maxima-mode maxima maxima-info maxima-start maxima-apropos)
  :custom
  (maxima-display-maxima-buffer nil))


;; Maxima mode with images
(use-package imaxima
  :load-path +maxima-path
  :when +maxima-available-p
  :commands (imaxima imath-mode)
  :hook (imaxima-startup . maxima-inferior-mode) ; To get syntax highlighting
  :custom
  (imaxima-use-maxima-mode-flag nil))


;; Preview TeX math equations inline
(use-package math-preview ; Needed by ein to render equations
  :straight t)


;; Jupyter notebook client in Emacs
(use-package ein
  :straight t
  :custom
  (ein:output-area-inlined-images t)
  :config
  (setq-default ein:markdown-enable-math t)
  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages (append org-babel-load-languages '((ein . t))))
    (cl-callf append org-src-lang-modes '(("ein-python" . python) ("ein-r" . r) ("ein-julia" . julia)))))


;; Lightweight notebooks in Emacs
(use-package code-cells
  :straight t
  :init
  ;; Both `ein' and `code-cells' registers auto-mode for ".ipynb" files,
  ;; we remove `code-cells' so `ein' gets used by default.
  (unless (+package-disabled-p 'ein)
    (setq auto-mode-alist (delete (rassoc 'code-cells-convert-ipynb auto-mode-alist) auto-mode-alist))))


;; An interface to communicate with Jupyter kernels
(use-package jupyter
  :straight t)


;; Julia support in Emacs
(use-package julia-mode
  :straight t)


;; Emacs Speaks Statistics
(use-package ess
  :straight t)


;; View R dataframes in a spreadsheet software
(use-package ess-view
  :straight t)


;; Data viewer for GNU R
(use-package ess-R-data-view
  :straight t)


(provide 'me-math)

;;; me-math.el ends here
