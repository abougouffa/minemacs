;;; me-math.el --- Mathematics stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(defconst +maxima-path "/usr/share/emacs/site-lisp/maxima/")
(defconst +maxima-available-p (and (executable-find "maxima") (file-directory-p +maxima-path)))

(use-package maxima
  :load-path +maxima-path
  :when +maxima-available-p
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :commands (inferior-maxima-mode maxima maxima-info maxima-start maxima-apropos)
  :custom
  (maxima-display-maxima-buffer nil))

(use-package imaxima
  :load-path +maxima-path
  :when +maxima-available-p
  :commands (imaxima imath-mode)
  :hook (imaxima-startup . maxima-inferior-mode) ; To get syntax highlighting
  :custom
  (imaxima-use-maxima-mode-flag nil))

(use-package matlab-mode
  :straight (:url "https://git.code.sf.net/p/matlab-emacs/src")
  :when (executable-find "matlab"))

(use-package math-preview ; Needed by ein to render equations
  :straight t)

(use-package ein
  :straight t
  :custom
  (ein:output-area-inlined-images t)
  :config
  (setq-default ein:markdown-enable-math t)
  (with-eval-after-load 'org
    (org-babel-do-load-languages 'org-babel-load-languages (append org-babel-load-languages '((ein . t))))
    (cl-callf append org-src-lang-modes '(("ein-python" . python) ("ein-r" . r) ("ein-julia" . julia)))))

(use-package code-cells
  :straight t
  :init
  ;; Both `ein' and `code-cells' registers auto-mode for ".ipynb" files,
  ;; we remove `code-cells' so `ein' gets used by default.
  (unless (+package-disabled-p 'ein)
    (setq auto-mode-alist (delete (rassoc 'code-cells-convert-ipynb auto-mode-alist) auto-mode-alist))))

(use-package jupyter
  :straight t)

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
