;;; me-debug.el --- Debugging stuff -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (concat "abougouffa" "@" "fedora" "project" "." "org")

;;; Commentary:

;;; Code:

(use-package realgud
  :straight (realgud :build (:not compile))
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map python-mode-map
                          c-ts-mode-map c++-ts-mode-map python-ts-mode-map
                          rust-mode-map rust-ts-mode-map
                          sh-mode-map bash-ts-mode-map)
    "r" '(nil :wk "realgud")
    "rd" #'+realgud:start
    "rh" #'+realgud-hydra/body))

(use-package realgud-lldb
  :straight t
  :init
  (defalias 'realgud:lldb #'realgud--lldb)
  :commands (realgud--lldb realgud:lldb lldb))

(use-package realgud-ipdb
  :straight t
  :commands (ipdb realgud:ipdb))

(defconst +objdump-available-p (executable-find "objdump"))

(use-package disaster
  :straight t
  :when +objdump-available-p
  :init
  (+map-local! :keymaps '(c-mode-map c++-mode-map fortran-mode-map)
    "D" #'disaster))


(provide 'me-debug)

;;; me-debug.el ends here
