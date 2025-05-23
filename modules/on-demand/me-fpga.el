;;; me-fpga.el --- FPGA modes (Verilog and VHDL) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-04-30

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-fpga
  :auto-mode '(("\\.vhdl?\\'" . vhdl-ts-mode)
               ("\\.[ds]?va?h?\\'" . verilog-ts-mode))
  :companion-packages '((vhdl-mode . vhdl-ts-mode) (verilog-mode . verilog-ts-mode)))


;; VHDL Tree-sitter major mode
(use-package vhdl-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter))


;; Verilog Tree-sitter major mode
(use-package verilog-ts-mode
  :straight t
  :when (featurep 'feat/tree-sitter))


(provide 'on-demand/me-fpga)
;;; me-fpga.el ends here
