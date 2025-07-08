;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-07-08

;;; Commentary:

;;; Code:


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit
  :straight `( :host github :repo "akirakyle/emacs-webkit"
               :files (:defaults "*.js" "*.css" "*.so")
               :pre-build ,(if (featurep 'os/win)
                               '(message "The `webkit' module won't build on Windows.")
                             '("make")))
  :when (and (featurep 'feat/modules) (not (featurep 'os/win))))


;; A Dynamic Module for WebKit, aka a fully fledged browser inside Emacs
(use-package webkit-dark
  :straight webkit
  :when (and (featurep 'feat/modules) (not (featurep 'os/win)))
  :bind (:map webkit-mode-map ("C-c d" . webkit-dark-toggle)))


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
  :config
  (with-eval-after-load 'consult
    (consult-customize consult-dash :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t)))))


(provide 'me-experimental)
;;; me-experimental.el ends here
