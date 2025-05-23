;;; me-sql.el --- SQL extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-04-27

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-sql
  :companion-packages '((sql-mode . sqlup-mode)))


;; Upcase SQL words for you
(use-package sqlup-mode
  :straight t)


;; Flymake backend for sqlfluff
(use-package flymake-sqlfluff
  :straight t)


(provide 'on-demand/me-sql)
;;; me-sql.el ends here
