;;; me-reformatter.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa  (rot13 "noqryunx.obhtbhssn@tznvy.pbz")
;; Created: 2025-07-09
;; Last modified: 2025-12-24

;;; Commentary:

;;; Code:


;; Define commands which run reformatters on the current Emacs buffer
(use-package reformatter
  :ensure t
  :config
  (reformatter-define ref-black
    :program "black"
    :args `(,@(when (equal "pyi" (and buffer-file-name (file-name-extension buffer-file-name)))
                '("--pyi"))
            "-")
    :lighter "Black ")

  (reformatter-define ref-lua-format
    :program "lua-format"
    :args `(,@(when-let* ((indent (cond ((derived-mode-p 'lua-ts-mode) lua-ts-indent-offset)
                                        ((derived-mode-p 'lua-mode) lua-indent-level))))
                `(,(format "--indent-width=%d" indent)))
            ,@(unless indent-tabs-mode
                '("--no-use-tab")))
    :lighter "LuaFmt ")

  (reformatter-define ref-clang-format
    :program +clang-format-command
    :args `(,(+clang-format-get-style)
            "-assume-filename"
            ,(or (when-let* ((file (buffer-file-name))) (file-name-nondirectory file))
                 (when-let* ((ext (car (+clang-format-get-lang)))) (file-name-with-extension "dummy" ext))
                 "dummy.c"))
    :lighter "ClangFmt "))


(provide 'obsolete/me-reformatter)
;;; me-reformatter.el ends here
