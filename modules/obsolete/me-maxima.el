;;; me-maxima.el --- Integration with Maxima -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(defconst +maxima-path-p "/usr/share/emacs/site-lisp/maxima/")
(defconst +maxima-available-p (and (executable-find "maxima") (file-directory-p +maxima-path-p)))

(use-package maxima
  :load-path +maxima-path-p
  :when +maxima-available-p
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :commands inferior-maxima-mode
  :custom
  (maxima-display-maxima-buffer nil))

(use-package imaxima
  :load-path +maxima-path-p
  :when +maxima-available-p
  :commands imaxima imath-mode
  :hook (imaxima-startup . maxima-inferior-mode) ; To get syntax highlighting
  :custom
  (imaxima-use-maxima-mode-flag nil))


(provide 'obsolete/me-maxima)

;;; me-maxima.el ends here
