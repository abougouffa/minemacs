;;; me-clojure.el --- Clojure support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-clojure
  :auto-mode '(("\\.cljs\\'" . clojurescript-mode)
               (("\\.cljc\\'" "\\.\\(clj\\|cljd\\|dtm\\|edn\\|lpy\\)\\'" "\\(?:build\\|profile\\)\\.boot\\'") . clojure-mode))
  :interpreter-mode '(("bb" . clojure-mode)
                      ("nbb" . clojurescript-mode)))


;; Major mode for Clojure code
(use-package clojure-mode
  :straight t)


;; Clojure Interactive Development Environment that Rocks
(use-package cider
  :straight t)


(provide 'on-demand/me-clojure)
;;; me-clojure.el ends here
