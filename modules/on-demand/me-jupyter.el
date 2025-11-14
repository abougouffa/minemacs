;;; me-jupyter.el --- Jupyter integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-30
;; Last modified: 2025-11-14

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-jupyter
  :auto-mode '(("\\.ipynb\\'" . ein:ipynb-mode))
  :define-loader t)


;; Preview TeX math equations inline
(use-package math-preview ; Needed by ein to render equations
  :straight t)


;; Taken from: https://github.com/millejoh/emacs-ipython-notebook/pull/925#issuecomment-3443730354
;; Native compilation causes problems for pdb in ein so prevent native compilation for ein.
(with-eval-after-load 'comp
  (add-to-list 'native-comp-jit-compilation-deny-list "ein"))


;; Jupyter notebook client in Emacs
(use-package ein
  :straight (ein :host github :repo "natsirtguy/emacs-ipython-notebook" :branch "fix-polymode-compatibility")
  :custom
  (ein:output-area-inlined-images t)
  :config
  (setq-default ein:markdown-enable-math t)
  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages (append org-babel-load-languages '((ein . t))))
    (cl-callf append org-src-lang-modes '(("ein-python" . python) ("ein-r" . r) ("ein-julia" . julia)))))


;; An interface to communicate with Jupyter kernels
(use-package jupyter
  :straight t)


(provide 'on-demand/me-jupyter)
;;; me-jupyter.el ends here
