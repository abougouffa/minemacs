;;; me-opencl.el --- OpenCL support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-opencl 'opencl-c-mode
  :auto-mode '(("\\.cl\\'" . opencl-c-mode)))

(use-package opencl-c-mode
  :straight t
  :mode "\\.cl\\'")


(provide 'modes/me-opencl)
;;; me-opencl.el ends here
