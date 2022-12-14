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

(use-package math-preview ; Needed by ein to render equations
  :straight t
  :defer t)

(use-package ein
  :straight t
  :mode ("\\.ipynb\\'" . ein:ipynb-mode)
  :custom
  (ein:output-area-inlined-images t)
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
    "l" #'ein:gat-run-local)
  :config
  (setq-default ein:markdown-enable-math t)

  (with-eval-after-load 'org
    (add-to-list 'org-babel-load-languages '(ein . t))
    (setq org-src-lang-modes (append org-src-lang-modes '(("ein-python" . python) ("ein-r" . r) ("ein-julia" . julia))))))

(use-package julia-mode
  :straight t
  :defer t)

(use-package ess
  :straight t
  :defer t)

(use-package ess-view
  :straight t
  :defer t)

(use-package ess-R-data-view
  :straight t
  :defer t)

(use-package poly-R
  :straight t
  :defer t)

(use-package octave
  :straight (:type built-in)
  :mode ("\\.m\\'" . octave-mode)
  :config
  (defun +octave-eval-last-sexp ()
    "Evaluate Octave sexp before point and print value into current buffer."
    (interactive)
    (inferior-octave t)
    (let ((print-escape-newlines nil)
          (opoint (point)))
      (prin1
       (save-excursion
         (forward-sexp -1)
         (inferior-octave-send-list-and-digest
          (list (concat (buffer-substring-no-properties (point) opoint) "\n")))
         (mapconcat 'identity inferior-octave-output-list "\n")))))

  (with-eval-after-load 'eros
    (defun +eros-octave-eval-last-sexp ()
      "Wrapper for `+octave-eval-last-sexp' that overlays results."
      (interactive)
      (eros--eval-overlay
       (+octave-eval-last-sexp)
       (point)))

    (+map-local :keymaps 'octave-mode-map
      "e"  '(nil :wk "eval")
      "ee" #'+eros-octave-eval-last-sexp)))

(provide 'me-math)
