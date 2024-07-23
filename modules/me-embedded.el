;;; me-embedded.el --- Embedded systems stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

(use-package embed
  :straight (:host github :repo "xal-0/embed-el"))

(use-package arduino-mode
  :straight (:host github :repo "bookest/arduino-mode")
  :hook (arduino-mode . display-line-numbers-mode)
  :hook (arduino-mode . hs-minor-mode))

(use-package dts-mode
  :straight t)

(use-package virtual-dts-mode
  :straight (:host github :repo "connorfeeley/virtual-dts-mode"))

(use-package bitbake
  :straight (bitbake-modes :host bitbucket :repo "olanilsson/bitbake-modes")
  :hook (bitbake-mode . bitbake-electric-mode)
  :config
  (require 'bitbake-insert)
  (require 'bitbake-electric)
  :init
  (defun +bitbake-poky-sources (build-dir &optional include-native)
    "Get all source directories for BUILD-DIR. Optionally INCLUDE-NATIVE."
    ;; From the build-dir "yocto-ws/build-MACHINE/", this will extract all source
    ;; directories of these formats:
    ;; yocto-ws/build-MACHINE/tmp/work/aarch64-rdk-linux/procps/3.3.16-r0/git/
    ;; yocto-ws/build-MACHINE/tmp/work/aarch64-rdk-linux/ppp/2.4.7-r0/ppp-2.4.7/
    (let* (result
           (base-dir (expand-file-name "tmp/work/" build-dir))
           (arch-dirs (seq-filter #'file-directory-p (directory-files base-dir t "[^.][^.]?\\'"))))
      (dolist (arch-dir arch-dirs)
        (let* ((package-dirs (directory-files arch-dir t "[^.][^.]?\\'"))
               (package-dirs (if include-native package-dirs (seq-filter (lambda (dir) (not (string-suffix-p "-native" dir))) package-dirs))))
          (dolist (package-dir package-dirs)
            (let ((ver-dirs (directory-files package-dir t "[^.][^.]?\\'")))
              (dolist (ver-dir ver-dirs)
                (let* ((ver (string-trim-right (file-name-nondirectory (directory-file-name ver-dir)) "-r[[:digit:]]*$"))
                       (dir-git (expand-file-name "git/" ver-dir))
                       (dir-non-git (expand-file-name (format "%s-%s/" (file-name-nondirectory (directory-file-name package-dir)) ver) ver-dir)))
                  (cond ((file-directory-p dir-git)
                         (push dir-git result))
                        ((file-directory-p dir-non-git)
                         (push dir-non-git result)))))))))
      result))

  (defun +bitbake-insert-poky-sources (build-dir)
    "Insert poky source directories for BUILD-DIR."
    (interactive "D")
    (insert (string-join (+bitbake-poky-sources build-dir) "\n"))))

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

(use-package x86-lookup
  :straight t
  :custom
  (x86-lookup-browse-pdf-function 'x86-lookup-browse-pdf-pdf-tools)
  ;; Get manual from intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html
  (x86-lookup-pdf (concat minemacs-local-dir "intel-64-and-ia32-volumes-1234.pdf"))
  :config
  (unless (file-exists-p x86-lookup-pdf)
    (url-copy-file "https://cdrdv2.intel.com/v1/dl/getContent/671200" x86-lookup-pdf t)))

(use-package pcap-mode
  :straight t
  :mode (rx "." (or "pcap" "pcapng" "ntar") eol))

(use-package vhdl-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))

(use-package verilog-ts-mode
  :straight t
  :when (+emacs-features-p 'tree-sitter))


(provide 'me-embedded)

;;; me-embedded.el ends here
