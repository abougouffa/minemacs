;;; me-cc.el --- C/C++ extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cc
  :companion-packages '(((c-mode c++-mode c-ts-mode c++-ts-mode) . flymake-cppcheck)))

(use-package flymake-cppcheck
  :straight (:host github :repo "shaohme/flymake-cppcheck")
  :init
  (when (executable-find "cppcheck")
    (satch-add-hook '(c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook) #'flymake-cppcheck-setup)))


(provide 'on-demand/me-cc)
;;; me-cc.el ends here
