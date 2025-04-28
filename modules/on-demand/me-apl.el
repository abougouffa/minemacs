;;; me-apl.el --- APL language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-apl
  :auto-mode '(("\\.apl\\'" . gnu-apl-mode))
  :interpreter-mode '(("apl" . gnu-apl-mode)))


;; Major mode for GNU APL
(use-package gnu-apl-mode
  :straight t)


(provide 'on-demand/me-apl)
;;; me-apl.el ends here
