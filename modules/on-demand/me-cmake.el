;;; me-cmake.el --- CMake integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cmake
  :auto-mode '((("CMakeLists\\.txt\\'" "\\.cmake\\'") . cmake-mode))
  :companion-packages '((cmake-ts-mode . (cmake-font-lock cmake-mode))))


;; Major mode for editing CMake sources
(use-package cmake-mode
  :vc (:url "https://github.com/emacsmirror/cmake-mode")


;; Advanced, type aware, highlight support for CMake
(use-package cmake-font-lock
  :vc (:url "https://github.com/Lindydancer/cmake-font-lock")
  :custom
  (cmake-font-lock-modes '(cmake-mode cmake-ts-mode)))


(provide 'on-demand/me-cmake)
;;; me-cmake.el ends here
