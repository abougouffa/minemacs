;;; me-opencl.el --- OpenCL support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-opencl
  :auto-mode '(("\\.cl\\'" . opencl-c-mode)))


;; Major mode with for OpenCL kernels
(use-package opencl-c-mode
  :straight t
  :mode "\\.cl\\'")


(provide 'on-demand/me-opencl)
;;; me-opencl.el ends here
