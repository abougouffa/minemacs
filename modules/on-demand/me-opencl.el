;;; me-opencl.el --- OpenCL support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-opencl
  :auto-mode '(("\\.\\(?:clc?\\|opencl\\)'" . opencl-c-mode)))


;; Major mode with for OpenCL kernels
(use-package opencl-c-mode
  :straight t)


(provide 'on-demand/me-opencl)
;;; me-opencl.el ends here
