;;; me-math.el --- Mathematics stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package maxima
  :straight (:host github :repo "emacsmirror/maxima"
                   :files (:defaults
                           "keywords"
                           "poly-maxima.el"))
  :preface
  (defconst MAXIMA-P (executable-find "maxima"))
  :when MAXIMA-P
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :hook ((maxima-mode maxima-inferior-mode) . maxima-font-lock-setup)
  :commands maxima-inferior-mode
  :custom
  (maxima-display-maxima-buffer nil))

(use-package imaxima
  :straight (imaxima
             :host nil
             :repo "https://git.code.sf.net/p/maxima/code"
             :files ("interfaces/emacs/imaxima/*"))
  :preface
  (defconst MAXIMA-P (executable-find "maxima"))
  :when MAXIMA-P
  :commands imaxima imath-mode
  :hook (imaxima-startup . maxima-inferior-mode) ; To get syntax highlighting
  :custom
  (imaxima-use-maxima-mode-flag nil))

(use-package ein
  :straight t
  :defer t
  :mode ("\\.ipynb\\'" . ein:ipynb-mode)
  :general
  (+map
    :infix "o"
    "j" '(nil :wk "ein")
    "jr" #'ein:run
    "jl" #'ein:login
    "jf" #'ein:file-open
    "jn" #'ein:notebook-open)
  (+map-local :keymaps 'ein:ipynb-mode-map
    "o" #'ein:process-find-file-callback
    "O" #'ein:process-open-notebook
    "r" #'ein:gat-run-remote
    "l" #'ein:gat-run-local))

(use-package ess
  :straight t
  :defer t
  :init
  (unless (featurep 'julia-mode)
    (add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode))))

(use-package ess-R-data-view
  :straight t
  :defer t)

(use-package poly-R
  :straight t
  :defer t)


(provide 'me-math)
