;;; me-erlang.el --- Erlang language -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-erlang
  :auto-mode '((("\\.erl$" "\\.app\\.src$" "\\.escript" "\\.hrl$" "\\.xrl$" "\\.yrl" "/ebin/.+\\.app") . erlang-mode)))

(use-package erlang
  :straight t)


(provide 'on-demand/me-erlang)
;;; me-erlang.el ends here
