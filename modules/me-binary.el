;;; me-binary.el --- Stuff to work with binary files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; +binary-* are autoloaded
;; BUG: Loading continuously on `dirvish'.
;; BUG: Showing up randomly on `tramp' files.
(setq +binary-objdump-enable nil)
;; (add-to-list 'magic-fallback-mode-alist '(+binary-objdump-buffer-p . objdump-disassemble-mode) t)
(add-to-list 'magic-fallback-mode-alist '(+binary-hexl-buffer-p . +binary-hexl-mode-maybe) t)

(provide 'me-binary)

;;; me-binary.el ends here
