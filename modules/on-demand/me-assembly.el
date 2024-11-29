;;; me-assembly.el --- Assembler support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-assembly
  :auto-mode '(("\\.S\\'" . gas-mode)
               (("\\.asm\\'" "\\.inc\\'") . masm-mode)
               ("\\.mips\\'" . mips-mode)
               ("\\.riscv\\'" . riscv-mode)))


;;;###autoload
(defun +asm-ask-for-mode (mode)
  "Ask the MODE to run."
  (interactive
   (list (intern (format "%s-mode"
                         (let ((completion-extra-properties
                                `(:annotation-function ,(lambda (m) (concat " \t" (cdr (assoc m minibuffer-completion-table)))))))
                           (completing-read
                            "Assembly flavor for this file: "
                            '(("asm"  . "Default (builtin `asm-mode')")
                              ("fasm" . "Flat Assembler")
                              ("gas"  . "GNU Assembler")
                              ("masm" . "Microsoft Macro Assembler")
                              ("mips" . "MIPS Assembly")
                              ("nasm" . "Netwide Assembler")
                              ("riscv" . "RISC-V Assembly"))))))))
  (if (fboundp mode)
      (call-interactively mode)
    (user-error "`%s' is not available" mode)))


;; Major mode for MIPS assembly
(use-package mips-mode
  :ensure t)


;; Major mode for RISC V assembly
(use-package riscv-mode
  :ensure t)


;; Major mode for Flat Assembler
(use-package fasm-mode
  :ensure t)


;; Major mode for Microsoft Macro Assembler
(use-package masm-mode
  :ensure t)


;; Major mode for Netwide Assembler
(use-package nasm-mode
  :ensure t)


;; Major mode for GNU Assembler
(use-package gas-mode
  :ensure t)


(provide 'on-demand/me-assembly)
;;; me-assembly.el ends here
