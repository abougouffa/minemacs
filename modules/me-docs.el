;;; me-docs.el --- Documents (PDF, EPUB, DOC...) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-09-17
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; PDFGrep is an Emacs module providing "grep" comparable facilities but for PDF files
(use-package pdfgrep
  :ensure t
  :when (executable-find "pdfgrep")
  :commands (pdfgrep-mode pdfgrep)
  :custom
  (pdfgrep-options " -H -n -r --include \"*.pdf\" ")
  :config
  (pdfgrep-mode 1))


;; An Emacs major mode to read and browse RFC documents
(use-package rfc-mode
  :ensure t
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
  :ensure t
  :hook
  (minemacs-build-functions . tldr-update-docs)
  (tldr-mode . visual-line-mode)
  :custom
  (tldr-enabled-categories '("common" "linux" "netbsd" "openbsd" "freebsd" "osx" "windows")))


;; Emacs viewer for DevDocs, offline documentation for programming languages and libraries
(use-package devdocs
  :ensure t
  :when (featurep 'feat/libxml2))


;; Offline documentation browser using Dash/Zeal docsets
(use-package dash-docs
  :vc (:url "https://github.com/abougouffa/dash-docs")
  :custom
  (dash-docs-docsets-path (concat minemacs-local-dir "docsets/"))
  (dash-docs-browser-func #'eww-browse-url)
  :init
  (+setq-hook! (sh-mode bash-ts-mode) dash-docs-docsets '("Bash"))
  (+setq-hook! (python-mode python-ts-mode) dash-docs-docsets '("Python 3" "OpenCV Python" "NumPy" "SciPy" "PyTorch" "TensorFlow" "SymPy" "mypy"))
  (+setq-hook! (c-mode c-ts-mode) dash-docs-docsets '("C" "OpenCV" "OpenCV C" "GStreamer" "glibc"))
  (+setq-hook! (c++-mode c++-ts-mode) dash-docs-docsets '("C++" "OpenCV" "OpenCV C++" "GStreamer" "gtkmm" "glibc"))
  (+setq-hook! (json-mode json-ts-mode) dash-docs-docsets '("jq"))
  (+setq-hook! cuda-mode dash-docs-docsets '("CUDA"))
  (+setq-hook! opencl-c-mode dash-docs-docsets '("OpenCL")))


;; Integration of `consult' with `dash-docs'
(use-package consult-dash
  :ensure t
  :unless (+package-disabled-p 'consult 'me-completion)
  :config
  (with-eval-after-load 'consult
    (consult-customize consult-dash :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t)))))


(provide 'me-docs)

;;; me-docs.el ends here
