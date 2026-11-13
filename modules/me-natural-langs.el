;;; me-natural-langs.el --- Natural languages stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2022-10-02
;; Last modified: 2026-11-13

;;; Commentary:

;;; Code:


;; Just-in-time spell checker based on the Enchanted library
(use-package jinx
  :straight t
  :when (featurep 'feat/modules)
  :autoload jinx--load-module
  :hook (text-mode . +spellcheck-mode)
  :init
  (defun +jinx-load-module ()
    "Try to compile and load the jinx module and fail silently."
    (condition-case err
        (let ((display-buffer-alist ; Hide the compilation buffer
               (cons '("\\*jinx module compilation\\*" (display-buffer-no-window) (allow-no-window . t))
                     display-buffer-alist)))
          (jinx--load-module))
      (error (+log! (error-message-string err)) nil)
      (:success t))))


;; Distraction-free words correction with `flyspell' via `completing-read'
(use-package flyspell-correct
  :straight t)


(provide 'me-natural-langs)

;;; me-natural-langs.el ends here
