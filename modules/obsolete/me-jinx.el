;;; me-jinx.el --- Spell checking using libencnhat -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:


(use-package jinx
  :straight t
  :when (+emacs-features-p 'modules)
  :init
  (+map! "ts" #'jinx-mode)
  (+nvmap! "z=" #'jinx-correct)
  ;; Module compilation with libenchant should be easy on Linux, BSD and Mac OS,
  ;; and on Windows when the installation is done using MSYS2. When Emacs flavor
  ;; is MinGW, do not setup the hook by default.
  (when (or os/linux os/bsd os/mac (and os/win (string-suffix-p "msys" system-configuration)))
   (add-hook 'text-mode-hook #'jinx-mode)))


(provide 'obsolete/me-jinx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; me-cov.el ends here
