;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa <abougouffa@fedoraproject.org>


;;; Debug
(use-package realgud
  :straight t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map rust-mode-map python-mode-map)
    "d" `(,(+cmdfy!
            (pcase major-mode
             ('python-mode (realgud:pdb))
             ((or 'c-mode 'c++-mode) (realgud:gdb))))
          :wk "realgud"))
  :commands (realgud:gdb
             realgud:gud
             realgud:zshdb
             realgud:bashdb
             realgud:kshdb
             realgud:pdb
             realgud:pdb-remote))

(use-package realgud-lldb
  :straight t
  :general
  (+map-local :keymaps '(rust-mode-map)
    "d" `(#'realgud--lldb :wk "realgud"))
  :commands (realgud--lldb))

(use-package realgud-ipdb
  :straight t
  :commands (realgud:ipdb realgud:ipdb-remote))

(use-package disaster
  :straight t
  :general
  (+map-local :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)
