;;; me-logs.el --- Log files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-07
;; Last modified: 2025-07-07

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-logs
  :auto-mode '(("\\.log\\(?:\\.[0-9\\-]+\\)?\\'" . logview-mode))
  :define-loader t)


;; Emacs mode for viewing log files
(use-package logview
  :straight t
  :custom
  (logview-cache-filename (concat minemacs-cache-dir "logview-cache.extmap"))
  (logview-additional-timestamp-formats '(("RDK-CCSP" (java-pattern . "yyMMdd-HH:mm:ss.SSSSSS"))))
  (logview-additional-submodes '(("RDK-CCSP" (format . "TIMESTAMP [mod=NAME, lvl=LEVEL] [tid=THREAD]") (levels . "RDK-CCSP"))))
  (logview-additional-level-mappings '(("RDK-CCSP" (error "ERROR") (warning "WARN") (information "INFO") (debug "DEBUG") (trace "NOTICE")))))


(provide 'on-demand/me-logs)
;;; me-logs.el ends here
