;;; me-helpful.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-16
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; A better Emacs *help* buffer
(use-package helpful
  :ensure t
  :bind (("C-h h" . helpful-at-point) ; orig. `view-hello-file'
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol] . helpful-symbol)
         ([remap describe-function] . helpful-callable)
         ([remap describe-command] . helpful-command)
         ([remap describe-key] . helpful-key))
  :config
  ;; HACK: Showing the source code of the symbol in the help buffer isn't always
  ;; helpful, especially for big definitions.
  (advice-add 'helpful--source :override #'ignore))


(provide 'obsolete/me-helpful)
;;; me-helpful.el ends here
