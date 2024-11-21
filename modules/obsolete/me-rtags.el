;;; me-rtags.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:


;; A client/server indexer for C/C++/Objc[++] with integration for Emacs based on Clang
(use-package rtags
  :straight t
  :custom
  (rtags-use-bookmarks nil)
  (rtags-autostart-diagnostics t)
  (rtags-jump-to-first-match nil)
  (rtags-results-buffer-other-window t)
  ;; Rtags' binaries are renamed on some systems (like Debian)
  (rtags-rc-binary-name (cl-find-if #'executable-find (list rtags-rc-binary-name "rtags-rc")))
  (rtags-rdm-binary-name (cl-find-if #'executable-find (list rtags-rdm-binary-name "rtags-rdm"))))


;; RTags backend for `xref'
(use-package rtags-xref
  :straight t
  :commands (rtags-xref-enable))


(provide 'obsolete/me-rtags)
;;; me-rtags.el ends here
