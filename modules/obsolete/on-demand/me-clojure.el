;;; me-clojure.el --- Clojure support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2024-08-10
;; Last modified: 2026-11-21

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
  :ensure t)


;; Clojure Interactive Development Environment that Rocks
(use-package cider
  :ensure t)


(provide 'on-demand/me-clojure)
;;; me-clojure.el ends here
