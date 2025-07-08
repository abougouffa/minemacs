;;; me-experimental.el --- Experimental packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-12-11
;; Last modified: 2025-07-08

;;; Commentary:

;;; Code:


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
