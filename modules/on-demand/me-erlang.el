;;; me-erlang.el --- Erlang language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-12
;; Last modified: 2025-06-03

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-erlang
  :auto-mode '((("\\.erl$" "\\.app\\.src$" "\\.escript" "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app") . erlang-mode)))


(defconst +erlang-path (concat minemacs-on-demand-modules-dir "third-party/erlang/"))

;; Major modes for editing and running Erlang files
(use-package erlang
  :load-path +erlang-path
  :init
  (require 'erlang-start)) ; This file will define autoloads for the different Erlang commands


;; Integrate `erlang' with `flymake'
(use-package erlang-flymake
  :after erlang
  :demand)


(provide 'on-demand/me-erlang)
;;; me-erlang.el ends here
