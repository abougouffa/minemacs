;;; me-fpga.el --- FPGA modes (Verilog and VHDL) -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-fpga
  :auto-mode '(("\\.vhdl?\\'" . vhdl-ts-mode)
               ("\\.[ds]?va?h?\\'" . verilog-ts-mode))
  :companion-packages '((vhdl-mode . vhdl-ts-mode) (verilog-mode . verilog-ts-mode)))

(use-package vhdl-ts-mode
  :straight t
  :when (+emacs-options-p 'tree-sitter))

(use-package verilog-ts-mode
  :straight t
  :when (+emacs-options-p 'tree-sitter))


(provide 'on-demand/me-fpga)
;;; me-fpga.el ends here
