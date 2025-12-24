;;; me-sql.el --- SQL extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-sql
  :companion-packages '((sql-mode . sqlup-mode)))


;; Upcase SQL words for you
(use-package sqlup-mode
  :ensure t)


;; Flymake backend for sqlfluff
(use-package flymake-sqlfluff
  :ensure t)


(provide 'on-demand/me-sql)
;;; me-sql.el ends here
