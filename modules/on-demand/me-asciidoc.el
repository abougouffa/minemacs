;;; me-asciidoc.el --- AsciiDoc -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@cneebg.pbz")
;; Created: 2026-04-27
;; Last modified: 2026-04-27

;;; Commentary:

;;; Code:

;;;###autoload
(minemacs-register-on-demand-module 'me-asciidoc
  :auto-mode '(("\\.adoc\\'" . asciidoc-mode)))


;; Tree-sitter based major mode for AsciiDoc markup
(use-package asciidoc-mode
  :straight t
  :hook (minemacs-build-functions . asciidoc-install-grammars))


(provide 'on-demand/me-asciidoc)
;;; me-asciidoc.el ends here
