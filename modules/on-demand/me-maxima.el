;;; me-maxima.el --- Maxima integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-05-30
;; Last modified: 2025-07-07

;;; Commentary:

;;; Code:


;;;###autoload
(defconst +maxima-path (if-let* ((sys-path "/usr/share/emacs/site-lisp/maxima/")
                                 ((file-directory-p sys-path)))
                           sys-path
                         (mapcar (apply-partially #'concat minemacs-on-demand-modules-dir "third-party/maxima/")
                                 '("emaxima/" "imaxima/" "misc/"))))

;;;###autoload
(defconst +maxima-available-p (and (executable-find "maxima") t))

;;;###autoload
(minemacs-register-on-demand-module 'me-maxima
  :define-loader '+maxima-available-p
  :auto-mode '(("\\.ma[cx]\\'" . maxima-mode))
  :interpreter-mode '(("maxima" . maxima-mode)))


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
  :when (and +maxima-available-p (executable-find "latex"))
  :commands (imaxima imath-mode)
  :hook (imaxima-startup . maxima-inferior-mode) ; To get syntax highlighting
  :custom
  (imaxima-use-maxima-mode-flag nil))


(provide 'on-demand/me-maxima)
;;; me-maxima.el ends here
