;; -*- lexical-binding: t; -*-

(defconst MAXIMA-P (executable-find "maxima"))

(use-package maxima
  :straight (:host github :repo "emacsmirror/maxima"
                   :files (:defaults
                           "keywords"
                           "poly-maxima.el"))
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :when MAXIMA-P
  :commands (maxima-mode maxima-inferior-mode maxima)
  :init
  (setq maxima-font-lock-keywords-directory ;; a workaround to undo the straight workaround!
        (expand-file-name (format "straight/%s/maxima/keywords" straight-build-dir) straight-base-dir)))


(use-package imaxima
  :straight (:host nil :repo "https://git.code.sf.net/p/maxima/code"
                   :files ("interfaces/emacs/imaxima/*"))
  :when MAXIMA-P
  :commands (imaxima imath-mode)
  :custom
  (imaxima-use-maxima-mode-flag nil) ;; otherwise, it don't render equations with LaTeX.
  :config
  ;; Hook the `maxima-inferior-mode' to get Company completion.
  (add-hook 'imaxima-startup-hook #'maxima-inferior-mode))
