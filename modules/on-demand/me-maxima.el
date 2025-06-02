;;; me-maxima.el --- Maxima integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-30
;; Last modified: 2025-06-02

;;; Commentary:

;;; Code:


;;;###autoload
(defconst +maxima-path "/usr/share/emacs/site-lisp/maxima/")

;;;###autoload
(defconst +maxima-available-p (and (executable-find "maxima") (file-directory-p +maxima-path)))

;;;###autoload
(defun minemacs-maxima-load ()
  "Load the `on-demand/me-maxima' module."
  (interactive))


;; Major modes for writing Maxima code
(use-package maxima
  :load-path +maxima-path
  :when +maxima-available-p
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :interpreter ("maxima" . maxima-mode)
  :commands (inferior-maxima-mode maxima maxima-info maxima-start maxima-apropos)
  :custom
  (maxima-display-maxima-buffer nil))


;; Maxima mode with images
(use-package imaxima
  :load-path +maxima-path
  :when +maxima-available-p
  :commands (imaxima imath-mode)
  :hook (imaxima-startup . maxima-inferior-mode) ; To get syntax highlighting
  :custom
  (imaxima-use-maxima-mode-flag nil))


(provide 'obsolete/me-maxima)
;;; me-maxima.el ends here
