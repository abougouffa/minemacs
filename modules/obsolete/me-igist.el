;;; me-igist.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2026-11-21
;; Last modified: 2026-11-21

;;; Commentary:

;;; Code:


;; Work seamlessly with GitHub gists from Emacs
(use-package igist
  :ensure t
  :config
  (advice-add ; BUGFIX: Don't save the Gist unless it has been modified
   'igist-save-gist-buffer :around
   (+defun igist--check-if-modified:around-a (orig-fn buffer &optional callback)
     (when (igist-gist-modified-p buffer)
       (funcall orig-fn buffer callback)))))


(provide 'obsolete/me-igist)
;;; me-igist.el ends here
