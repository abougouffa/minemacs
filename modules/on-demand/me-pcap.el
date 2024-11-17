;;; me-pcap.el --- PCAP support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-pcap
  :auto-mode '(("\\.\\(?:ntar\\|pcap\\(?:ng\\)?\\)\\'" . pcap-mode)))


;; Major mode for working with PCAP files via Wireshark's `tshark' tool
(use-package pcap-mode
  :straight t
  :mode (rx "." (or "pcap" "pcapng" "ntar") eol)
  :config
  (+setq-hook! pcap-mode
    truncate-lines t))

(provide 'on-demand/me-pcap)
;;; me-pcap.el ends here
