;;; me-cmake.el --- CMake integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-11
;; Last modified: 2025-03-21

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cmake
  :auto-mode '((("CMakeLists\\.txt\\'" "\\.cmake\\'") . cmake-mode))
  :companion-packages '((cmake-ts-mode . (cmake-font-lock cmake-mode))))


;; Major mode for editing CMake sources
(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))


;; Advanced, type aware, highlight support for CMake
(use-package cmake-font-lock
  :straight (:host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :custom
  (cmake-font-lock-modes '(cmake-mode cmake-ts-mode)))


(provide 'on-demand/me-cmake)
;;; me-cmake.el ends here
