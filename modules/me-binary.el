;;; me-binary.el --- Stuff to work with binary files -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;; +binary-* are autoloaded
(with-eval-after-load 'minemacs-loaded
  (add-to-list 'magic-fallback-mode-alist '(+binary-objdump-buffer-p . objdump-disassemble-mode) t)
  (add-to-list 'magic-fallback-mode-alist '(+binary-hexl-buffer-p . +binary-hexl-mode-maybe) t))


(provide 'me-binary)
