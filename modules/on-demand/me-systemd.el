;;; me-systemd.el --- SystemD files -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Created: 2025-07-07
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-systemd
  :auto-mode `(("\\.nspawn\\'" . systemd-mode)
               (,(rx (+? (any "a-zA-Z0-9-_.@\\")) "."
                     (or "automount" "busname" "mount" "path" "service" "slice"
                         "socket" "swap" "target" "timer" "link" "netdev" "network")
                     string-end)
                . systemd-mode)
               (,(rx ".#" (or (and (+? (any "a-zA-Z0-9-_.@\\")) "."
                                   (or "automount" "busname" "mount" "path"
                                       "service" "slice" "socket" "swap" "target"
                                       "timer" "link" "netdev" "network"))
                              "override.conf")
                     (= 16 (char hex-digit))
                     string-end)
                . systemd-mode)
               (,(rx "/systemd/" (+? anything) ".d/" (+? (not (any 47))) ".conf" string-end) . systemd-mode)))


;; Major mode for editing systemd units
(use-package systemd
  :vc (:url "https://github.com/abougouffa/systemd-mode"))


(provide 'on-demand/me-systemd)
;;; me-systemd.el ends here
