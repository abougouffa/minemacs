;;; me-sql.el --- SQL extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn-rkg@fntrzpbz.pbz")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-sql
  :companion-packages '((sql-mode . sqlup-mode)))

(use-package sqlup-mode
  :straight t)


(provide 'on-demand/me-sql)
;;; me-sql.el ends here
