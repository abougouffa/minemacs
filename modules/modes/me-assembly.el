;;; me-assembly.el --- Assembler support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-assembly '(mips-mode riscv-mode fasm-mode masm-mode nasm-mode gas-mode)
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

(use-package mips-mode
  :straight t)

(use-package riscv-mode
  :straight t)

(use-package fasm-mode
  :straight t)

(use-package masm-mode
  :straight t)

(use-package nasm-mode
  :straight t)

(use-package gas-mode
  :straight t)


(provide 'modes/me-assembly)
;;; me-assembly.el ends here
