;;; me-docs.el --- Documents (PDF, EPUB, DOC...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-07-28

;;; Commentary:

;;; Code:

;; Emacs support library for PDF files
(use-package pdf-tools
  :straight t
  :mode ("\\.[pP][dD][fF]\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :hook (minemacs-build-functions . pdf-tools-install)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-image-relief 2)
  (pdf-view-use-scaling t))


;; PDFGrep is an Emacs module providing "grep" comparable facilities but for PDF files
(use-package pdfgrep
  :straight t
  :commands (pdfgrep-mode pdfgrep)
  :custom
  (pdfgrep-options " -H -n -r --include \"*.pdf\" ")
  :config
  (pdfgrep-mode 1))


;; An Emacs major mode to read and browse RFC documents
(use-package rfc-mode
  :straight t
  :custom
  (rfc-mode-directory (concat minemacs-local-dir "rfc"))
  :init
  ;; Use a window wide enough but not too wide
  (add-to-list
   'display-buffer-alist
   `((derived-mode . rfc-mode)
     (display-buffer-in-side-window)
     (slot . 0)
     (side . right)
     (dedicated . t) ;; Close when finished
     (window-width . 76))))


;; Browse "tldr" pages from Emacs
(use-package tldr
  :straight t
  :hook
  (minemacs-build-functions . tldr-update-docs)
  (tldr-mode . visual-line-mode)
  :custom
  (tldr-enabled-categories '("common" "linux" "netbsd" "openbsd" "freebsd" "osx" "windows")))


;; Emacs viewer for DevDocs, offline documentation for programming languages and libraries
(use-package devdocs
  :straight t
  :when (featurep 'feat/libxml2))


;; Offline documentation browser using Dash/Zeal docsets
(use-package dash-docs
  :straight (:host github :repo "abougouffa/dash-docs")
  :custom
  (dash-docs-docsets-path (concat minemacs-local-dir "docsets/"))
  (dash-docs-browser-func #'eww-browse-url)
  :init
  (+setq-hook! (sh-mode bash-ts-mode) dash-docs-docsets '("Bash"))
  (+setq-hook! (python-mode python-ts-mode) dash-docs-docsets '("Python 3" "OpenCV Python" "NumPy" "SciPy" "PyTorch" "TensorFlow" "SymPy" "mypy"))
  (+setq-hook! (c-mode c-ts-mode) dash-docs-docsets '("C" "OpenCV" "OpenCV C"))
  (+setq-hook! (c++-mode c++-ts-mode) dash-docs-docsets '("C++" "OpenCV" "OpenCV C++"))
  (+setq-hook! (json-mode json-ts-mode) dash-docs-docsets '("jq"))
  (+setq-hook! cuda-mode dash-docs-docsets '("CUDA"))
  (+setq-hook! opencl-c-mode dash-docs-docsets '("OpenCL")))


;; Integration of `consult' with `dash-docs'
(use-package consult-dash
  :straight t
  :unless (+package-disabled-p 'consult 'me-completion)
  :config
  (with-eval-after-load 'consult
    (consult-customize consult-dash :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t)))))


(provide 'me-docs)

;;; me-docs.el ends here
