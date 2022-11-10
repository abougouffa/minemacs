;; -*- lexical-binding: t; -*-

;; +binary-* are autoloaded
(with-eval-after-load 'minemacs-loaded
  (add-to-list 'magic-fallback-mode-alist '(+binary-objdump-buffer-p . objdump-disassemble-mode) t)
  (add-to-list 'magic-fallback-mode-alist '(+binary-hexl-buffer-p . +binary-hexl-mode-maybe) t))
