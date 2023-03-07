;; me-gc.el --- MinEmacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


(use-package gcmh
  :straight t
  :hook (minemacs-lazy . gcmh-mode)
  :custom
  ;; Set the delay to 20s instead of the default 15. I tried using `auto', but
  ;; with the default 20 of `gcmh-auto-idle-delay-factor', it make trigger GC
  ;; each 1s on my machine.
  (gcmh-idle-delay 20)
  ;; The default `gcmh's 1GB is probably too high. We set it to 256MB on 64bit
  ;; systems, or 16MB on 32bit ones.
  (gcmh-high-cons-threshold
   (* 1024 1024 (if (memq sys/arch '(x86_64 amd64 aarch64)) 256 16))))


(provide 'me-gc)
