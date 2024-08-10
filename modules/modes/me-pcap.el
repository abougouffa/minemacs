;;; me-pcap.el --- PCAP support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-extra-mode 'me-pcap 'pcap-mode
  :auto-mode '(("\\.\\(?:ntar\\|pcap\\(?:ng\\)?\\)\\'" . pcap-mode)))

(use-package pcap-mode
  :straight t
  :mode (rx "." (or "pcap" "pcapng" "ntar") eol))


(provide 'modes/me-pcap)
;;; me-pcap.el ends here
