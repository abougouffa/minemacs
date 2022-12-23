;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;;; Debug
(use-package realgud
  :straight t
  :defer t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map python-mode-map
                         sh-mode-map bash-ts-mode-map)
    "d" `(,(+cmdfy!
            (pcase major-mode
             ((or 'python-mode 'python-ts-mode) (realgud:pdb))
             ((or 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode) (realgud:gdb))
             ((or 'bash-ts-mode 'sh-mode) (realgud:bashdb))))
          :wk "realgud")))

(use-package realgud-lldb
  :straight t
  :general
  (+map-local :keymaps '(rust-mode-map)
    "d" `(#'realgud--lldb :wk "realgud"))
  :commands realgud--lldb)

(use-package realgud-ipdb
  :straight t
  :defer t)

(use-package disaster
  :straight t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)
