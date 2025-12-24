;;; me-cc.el --- C/C++ extras -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-15
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cc
  :companion-packages '(((c-mode c++-mode c-ts-mode c++-ts-mode) . flymake-cppcheck)))


;; Flymake backend for CppCheck
(use-package flymake-cppcheck
  :vc (:url "https://github.com/shaohme/flymake-cppcheck")
  :init
  (when (executable-find "cppcheck")
    (satch-add-hook '(c-mode-hook c-ts-mode-hook c++-mode-hook c++-ts-mode-hook) #'flymake-cppcheck-setup)))


(provide 'on-demand/me-cc)
;;; me-cc.el ends here
