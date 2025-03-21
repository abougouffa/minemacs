;;; me-pcap.el --- PCAP support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-pcap
  :auto-mode '(("\\.\\(?:5vw\\|apc\\|bfr\\|cap\\|dmp\\|eth\\|fdc\\|mplog\\|n\\(?:cf\\|tar\\)\\|p\\(?:cap\\(?:ng\\)?\\|kt\\)\\|rf5\\|s\\(?:noop\\|yc\\)\\|t\\(?:pc\\|r\\(?:c[01]\\|[1c]\\)\\)\\|vn\\(?:tc\\)?\\|wpz\\)$" . pcap-mode)))


;; Major mode for working with PCAP files via Wireshark's `tshark' tool
(use-package pcap-mode
  :straight t
  ;; An extensive list of extensions can be found at: https://tshark.dev/formats/magic_numbers
  :mode (rx "." (or "pcap" "pcapng" "ntar" "5vw" "ncf" "tr1" "trc" "trc0" "trc1"
                    "cap" "fdc" "syc" "bfr" "dmp" "rf5" "snoop" "eth" "pkt"
                    "tpc" "apc" "wpz" "vntc" "vn" "mplog")
            eol)
  :bind (:map pcap-mode-map ("n" . next-line) ("p" . previous-line))
  :config
  (+setq-hook! pcap-mode
    truncate-lines t))


(provide 'on-demand/me-pcap)
;;; me-pcap.el ends here
