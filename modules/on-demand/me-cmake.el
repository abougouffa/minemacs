;;; me-cmake.el --- CMake integration -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-cmake
  :auto-mode '((("CMakeLists\\.txt\\'" "\\.cmake\\'") . cmake-mode))
  :companion-packages '((cmake-ts-mode . (cmake-font-lock cmake-mode))))

(use-package cmake-mode
  :straight (:host github :repo "emacsmirror/cmake-mode" :files (:defaults "*")))

(use-package cmake-font-lock
  :straight (:host github :repo "Lindydancer/cmake-font-lock" :files (:defaults "*"))
  :custom
  (cmake-font-lock-modes '(cmake-mode cmake-ts-mode)))


(provide 'on-demand/me-cmake)
;;; me-cmake.el ends here
