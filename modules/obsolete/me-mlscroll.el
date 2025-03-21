;;; me-mlscroll.el --- Mode line scroll bar -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package mlscroll
  :straight t
  :hook (minemacs-lazy . +mlscroll-right-mode)
  :config
  ;; For `doom-modeline'
  (define-minor-mode +mlscroll-right-mode
    "Minor mode for displaying an interactive scrollbar in the mode line."
    :global t
    (if +mlscroll-right-mode
        (progn
          (setq mlscroll-right-align nil)
          (add-to-list 'global-mode-string '("" (:eval (mlscroll-mode-line)) " "))
          (mlscroll-layout)
          (add-hook 'enable-theme-functions #'mlscroll-layout)
          (add-hook 'after-make-frame-functions #'mlscroll--update-size)
          (when mlscroll-shortfun-min-width (mlscroll-shortfun-setup)))
      (cl-callf2 delete '("" (:eval (mlscroll-mode-line)) " ") global-mode-string)
      (remove-hook 'after-make-frame-functions #'mlscroll--update-size))))


(provide 'obsolete/me-mlscroll)
;;; me-mlscroll.el ends here
