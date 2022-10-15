;; -*- lexical-binding: t; -*-

(defconst MAXIMA-P (executable-find "maxima"))

(use-package maxima
  :straight (:host github :repo "emacsmirror/maxima"
                   :files (:defaults
                           "keywords"
                           "poly-maxima.el"))
  :when MAXIMA-P
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :hook ((maxima-mode maxima-inferior-mode) . maxima-font-lock-setup)
  :custom
  (maxima-display-maxima-buffer nil))


(use-package imaxima
  :straight (:host nil :repo "https://git.code.sf.net/p/maxima/code"
                   :files ("interfaces/emacs/imaxima/*"))
  :when MAXIMA-P
  :commands (imaxima imath-mode)
  :custom
  (setq imaxima-use-maxima-mode-flag nil)
  :config
  ;; Hook the `maxima-inferior-mode' to get syntax highlighting
  (add-hook 'imaxima-startup-hook #'maxima-inferior-mode))
