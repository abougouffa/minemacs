;;; me-math.el --- Mathematics stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package math-preview ; Needed by ein to render equations
  :straight t)

(use-package ein
  :straight t
  :custom
  (ein:output-area-inlined-images t)
  :init
  (+map! :infix "o"
    "j" '(nil :wk "ein")
    "jr" #'ein:run
    "jl" #'ein:login
    "jf" #'ein:file-open
    "jn" #'ein:notebook-open)
  :config
  (+map-local! :keymaps 'ein:ipynb-mode-map
    "o" #'ein:process-find-file-callback
    "O" #'ein:process-open-notebook
    "r" #'ein:gat-run-remote
    "l" #'ein:gat-run-local)

  (setq-default ein:markdown-enable-math t)

  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages (append org-babel-load-languages '((ein . t))))
    (setq org-src-lang-modes (append org-src-lang-modes '(("ein-python" . python) ("ein-r" . r) ("ein-julia" . julia))))))

(use-package code-cells
  :straight t
  :init
  ;; Both `ein' and `code-cells' registers auto-mode for ".ipynb" files,
  ;; we remove `code-cells' so `ein' gets used by default.
  (setq auto-mode-alist (delete (rassoc 'code-cells-convert-ipynb auto-mode-alist) auto-mode-alist)))

(use-package julia-mode
  :straight t)

(use-package ess
  :straight t)

(use-package ess-view
  :straight t)

(use-package ess-R-data-view
  :straight t)


(provide 'me-math)

;;; me-math.el ends here
