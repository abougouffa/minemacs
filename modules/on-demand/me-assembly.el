;;; me-assembly.el --- Assembler support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-assembly
  :auto-mode '(("\\.S\\'" . gas-mode)
               (("\\.asm\\'" "\\.inc\\'") . masm-mode)
               ("\\.mips\\'" . mips-mode)
               ("\\.riscv\\'" . riscv-mode))
  :companion-packages '((asm-mode . x86-lookup)))


;;;###autoload
(defun +asm-ask-for-mode (mode)
  "Ask the MODE to run."
  (interactive
   (list (intern (format "%s-mode"
                         (let ((completion-extra-properties
                                `(:annotation-function
                                  ,(lambda (m)
                                     (concat (make-string (- 10 (length m)) ?\ )
                                             (propertize (alist-get m minibuffer-completion-table nil nil #'equal)
                                                         'face 'font-lock-comment-face))))))
                           (completing-read
                            "Assembly flavor for this file: "
                            '(("asm"  . "Default (builtin `asm-mode')")
                              ("fasm" . "Flat Assembler")
                              ("masm" . "Microsoft Macro Assembler")
                              ("mips" . "MIPS Assembly")
                              ("nasm" . "Netwide Assembler")
                              ("riscv" . "RISC-V Assembly")
                              ("arm" . "Advanced RISC Machine (ARM) Assembly"))))))))
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
  :vc (:url "https://github.com/emacsattic/fasm-mode"))


;; Major mode for Microsoft Macro Assembler
(use-package masm-mode
  :ensure t)


;; Major mode for Netwide Assembler
(use-package nasm-mode
  :ensure t)


;; Major mode for editing Advanced RISC Machine (a.k.a. ARM) assembly code
(use-package arm-mode
  :vc (:url "https://github.com/charJe/arm-mode")
  :commands (arm-mode))


;; Quickly jump to Intel's x86 documentation from Emacs
(use-package x86-lookup
  :ensure t
  :custom
  ;; Get manual from https://intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (x86-lookup-pdf (concat minemacs-local-dir "intel-64-and-ia32-volumes-1234.pdf"))
  (x86-lookup-cache-directory (concat minemacs-cache-dir "x86-lookup/"))
  :config
  (unless (file-exists-p x86-lookup-pdf)
    (url-copy-file "https://cdrdv2.intel.com/v1/dl/getContent/671200" x86-lookup-pdf t)))


(provide 'on-demand/me-assembly)
;;; me-assembly.el ends here
