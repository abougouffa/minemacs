;;; me-logs.el --- Log files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-07
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-logs
  :auto-mode '(("\\.log\\(?:\\.[0-9\\-]+\\)?\\'" . logview-mode))
  :define-loader t)


;; Emacs mode for viewing log files
(use-package logview
  :ensure t
  :custom
  (logview-cache-filename (concat minemacs-cache-dir "logview-cache.extmap"))
  (logview-additional-timestamp-formats
   '(("RDK-CCSP" (java-pattern . "yyMMdd-HH:mm:ss.SSSSSS"))
     ("ISO 8601 datetime + millis, no year" (java-pattern . "MM-dd HH:mm:ss.SSS"))))
  (logview-additional-submodes
   `(("RDK-CCSP"
      (format . "TIMESTAMP [mod=NAME, lvl=LEVEL] [tid=THREAD]")
      (levels . "RDK-CCSP")
      (timestamp . "RDK-CCSP"))
     ("ulogcat-minimal"
      (format . "LEVEL <<RX:NAME:[^[:space:]()]*>>: MESSAGE")
      (levels . "ulogcat"))
     ("ulogcat-std"
      (format . "LEVEL <<RX:NAME:[^[:space:]()]*>><<RX:IGNORED:[[:space:]]*>>(THREAD)<<RX:IGNORED:[[:space:]]*>>: MESSAGE")
      (levels . "ulogcat"))
     ("ulogcat-long"
      (format . ,(concat
                  "<<RX:IGNORED:\\([UK][[:space:]]\\)?>>"
                  "TIMESTAMP LEVEL "
                  "<<RX:NAME:[^[:space:]()]*>>"
                  "<<RX:IGNORED:[[:space:]]*>>"
                  "<<RX:THREAD:\\(([^[:space:]])\\)?*>>"
                  "<<RX:IGNORED:[[:space:]]*>>: MESSAGE"))
      (levels . "ulogcat")
      (timestamp . "ISO 8601 datetime + millis, no year"))))
  (logview-additional-level-mappings
   '(("RDK-CCSP" (error "ERROR") (warning "WARN") (information "INFO") (debug "DEBUG") (trace "NOTICE"))
     ("ulogcat" (error "E" "C") (warning "W") (information "I" "N") (debug "D") (trace)))))


(provide 'on-demand/me-logs)
;;; me-logs.el ends here
